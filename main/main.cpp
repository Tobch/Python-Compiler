#include <windows.h>
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <stack>
#include <cctype>
#include <map>
#include <unordered_map>
#include <commctrl.h>
#include <iostream>
#include <memory>
#pragma comment(lib, "comctl32.lib")

using namespace std;

// Control IDs for GUI elements
#define IDC_INPUT          101
#define IDC_OUTPUT         102
#define IDC_SYMBOL         103
#define IDC_ERRORS         104
#define IDC_LEX_BUTTON     105
#define IDC_PARSE_BUTTON   106
#define IDC_PARSE_TREE     107
#define IDC_SYNTAX_ERRORS  108

// Structure to represent a token.
struct Token {
    string type;   // e.g., "KEYWORD", "IDENTIFIER", "NUMBER", "STRING", "INDENT", "DEDENT", "NEWLINE", "EOF"
    string value;  // the lexeme
    int line;
    int column;
};

// Structure for Symbol (Identifier in symbol table).
struct Symbol {
    string name;    // Identifier name (e.g., 'a', 'b', 'c')
    string type;    // Type of the identifier (e.g., 'INTEGER', 'STRING')
    string value;   // Value of the identifier (e.g., '5', '"hello"')

    Symbol(string n, string t, string v) : name(n), type(t), value(v) {}
};

// List of Python keywords.
vector<string> keywords = {
    "if", "else", "elif", "while", "for", "break", "continue", "pass",
    "def", "class", "lambda", "try", "except", "finally", "raise",
    "import", "from", "as", "return", "yield", "global", "nonlocal",
    "assert", "del", "in", "is", "not", "or", "and", "True", "False", "None","print"
};

// Forward declarations
bool isKeyword(const string& word);
void parseLine(const string& line, int lineNumber, int startColumn,
    vector<Token>& tokens, vector<string>& errors,
    vector<Symbol>& symbolTable);

// Checks if a word is a Python keyword.
bool isKeyword(const string& word) {
    return find(keywords.begin(), keywords.end(), word) != keywords.end();
}

bool isFunctionDefinition(const string& line, size_t pos) {
    size_t defPos = line.find("def", 0);
    return defPos != string::npos && defPos < pos;
}

// Splits the source code into lines.
vector<string> splitIntoLines(const string& source) {
    vector<string> lines;
    size_t start = 0;
    while (true) {
        size_t pos = source.find('\n', start);
        if (pos == string::npos) {
            lines.push_back(source.substr(start));
            break;
        }
        else {
            lines.push_back(source.substr(start, pos - start));
            start = pos + 1;
        }
    }
    return lines;
}

// Main tokenize function
vector<Token> tokenize(const string& source, vector<string>& errors, vector<Symbol>& symbolTable) {
    vector<Token> tokens;
    vector<string> lines = splitIntoLines(source);
    stack<int> indentStack;
    indentStack.push(0);

    // Find the last non-empty line
    int lastNonEmptyLine = -1;
    for (int i = 0; i < (int)lines.size(); ++i) {
        string lineCopy = lines[i];
        lineCopy.erase(remove(lineCopy.begin(), lineCopy.end(), '\r'), lineCopy.end());
        lineCopy.erase(0, lineCopy.find_first_not_of(' ')); // Trim leading spaces
        if (!lineCopy.empty()) {
            lastNonEmptyLine = i;
        }
    }

    for (int lineNumber = 0; lineNumber < (int)lines.size(); ++lineNumber) {
        string lineCopy = lines[lineNumber];
        lineCopy.erase(remove(lineCopy.begin(), lineCopy.end(), '\r'), lineCopy.end());

        int leadingSpaces = 0;
        while (leadingSpaces < (int)lineCopy.size() && lineCopy[leadingSpaces] == ' ')
            leadingSpaces++;

        int currentIndent = indentStack.top();
        if (leadingSpaces > currentIndent) {
            indentStack.push(leadingSpaces);
            tokens.push_back({ "INDENT", string(leadingSpaces, ' '), lineNumber + 1, 1 });
        }
        else if (leadingSpaces < currentIndent) {
            while (!indentStack.empty() && leadingSpaces < indentStack.top()) {
                indentStack.pop();
                tokens.push_back({ "DEDENT", "", lineNumber + 1, 1 });
            }
            if (!indentStack.empty() && indentStack.top() != leadingSpaces)
                errors.push_back("Inconsistent indentation at line " + to_string(lineNumber + 1));
        }

        parseLine(lineCopy, lineNumber + 1, leadingSpaces + 1, tokens, errors, symbolTable);

        // Add NEWLINE only for non-empty lines or before the last non-empty line
        lineCopy.erase(0, lineCopy.find_first_not_of(' ')); // Trim leading spaces
        if (!lineCopy.empty() && lineNumber < lastNonEmptyLine) {
            tokens.push_back({ "NEWLINE", "\n", lineNumber + 1, (int)lines[lineNumber].size() + 1 });
        }
    }

    // Add final DEDENTs
    while (indentStack.size() > 1) {
        indentStack.pop();
        tokens.push_back({ "DEDENT", "", (int)lines.size(), 1 });
    }

    tokens.push_back({ "EOF", "", (int)lines.size(), 1 });
    return tokens;
}

int findClosingMatch(const string& expr, int start, char openChar, char closeChar) {
    int count = 0;
    for (int i = start; i < expr.length(); i++) {
        if (expr[i] == openChar) count++;
        else if (expr[i] == closeChar) count--;
        if (count == 0) return i;
    }
    return -1; // No matching closing character found
}

bool isValidHex(const string& num) {
    for (size_t i = 2; i < num.length(); ++i) {
        if (!isxdigit(num[i])) return false;
    }
    return true;
}

bool isValidBinary(const string& num) {
    for (size_t i = 2; i < num.length(); ++i) {
        if (num[i] != '0' && num[i] != '1') return false;
    }
    return true;
}

bool isValidFloat(const string& num) {
    int dotCount = 0;
    for (char c : num) {
        if (c == '.') dotCount++;
        else if (!isdigit(c)) return false;
    }
    return dotCount <= 1;
}

// Line parser
void parseLine(const string& line, int lineNumber, int startColumn,
    vector<Token>& tokens, vector<string>& errors,
    vector<Symbol>& symbolTable) {
    int i = 0, column = startColumn;
    bool lastTokenWasNumberOrIdentifier = false;
    bool seenDef = false; // track if we just saw 'def'

    stack<pair<char, int>> delimiterStack; // (char, column)
    static bool inBlockComment = false;  // Flag to track whether we're inside a block comment
    while (i < (int)line.size()) {
        char c = line[i];
        // Check for the start of a block comment (""")
        if (!inBlockComment && i + 2 < line.size() && line[i] == '"' && line[i + 1] == '"' && line[i + 2] == '"') {
            inBlockComment = true;
            i += 3;  // Skip past the starting """
            column += 3;  // Move the column forward
            continue;  // Skip the rest of the line while in the comment
        }

        // Check for the end of a block comment (""")
        if (inBlockComment && i + 2 < line.size() && line[i] == '"' && line[i + 1] == '"' && line[i + 2] == '"') {
            inBlockComment = false;
            i += 3;  // Skip past the ending """
            column += 3;  // Move the column forward
            continue;  // Skip the rest of the line while in the comment
        }

        if (inBlockComment) {
            i++; column++;  // Skip characters inside the block comment
            continue;
        }
        // Handle comma
        if (c == ',') {
            int startCol = column;
            tokens.push_back({ "Comma", ",", lineNumber, startCol });
            i++; column++;
            continue;
        }
        if (c == '#') {
            // Ignore the rest of the line as it's a comment
            break;
        }
        // Handle colon
        if (c == ':') {
            int startCol = column;
            tokens.push_back({ "Colon", ":", lineNumber, startCol });
            i++; column++;
            continue;
        }

        if (c == ' ') {
            i++; column++;
            continue;
        }
        if (c == '<') {
            int startCol = column;
            string op(1, c);  // Single character '<'

            // Check for '<=' operator
            if (i + 1 < line.size() && line[i + 1] == '=') {
                op += '=';  // Append '=' to form the '<=' operator
                i++; column++;  // Skip the next character
            }

            tokens.push_back({ "OPERATOR", op, lineNumber, startCol });
            i++; column++;  // Move to the next character
            continue;
        }

        if (c == '>') {
            int startCol = column;
            string op(1, c);  // Single character '>'

            // Check for '>=' operator
            if (i + 1 < line.size() && line[i + 1] == '=') {
                op += '=';  // Append '=' to form the '>=' operator
                i++; column++;  // Skip the next character
            }

            tokens.push_back({ "OPERATOR", op, lineNumber, startCol });
            i++; column++;  // Move to the next character
            continue;
        }

        if (c == '!') {
            int startCol = column;
            string op(1, c);  // Single character '!'

            // Check for '!=' operator
            if (i + 1 < line.size() && line[i + 1] == '=') {
                op += '=';  // Append '=' to form the '!=' operator
                i++; column++;  // Skip the next character
            }

            tokens.push_back({ "OPERATOR", op, lineNumber, startCol });
            i++; column++;  // Move to the next character
            continue;
        }

        if (c == '=') {
            int startCol = column;
            string op(1, c);  // Single character '='

            // Check for '==' operator
            if (i + 1 < line.size() && line[i + 1] == '=') {
                op += '=';  // Append '=' to form the '==' operator
                i++; column++;  // Skip the next character
            }

            tokens.push_back({ "OPERATOR", op, lineNumber, startCol });
            i++; column++;  // Move to the next character
            continue;
        }

        if (c == '-') {
            int startCol = column;
            string number = "-";
            int temp = i + 1;
            bool isFloat = false, malformed = false;
            int dotCount = 0;

            // Look ahead to see if it's a number (digits or decimals)
            while (temp < line.size() && (isdigit(line[temp]) || line[temp] == '.')) {
                if (line[temp] == '.') {
                    dotCount++;
                    if (dotCount > 1) {
                        malformed = true;
                        break;
                    }
                }
                number += line[temp];
                temp++;
            }

            if (number.length() > 1 && isdigit(number[1])) {
                if (malformed) {
                    errors.push_back("Invalid number format: multiple decimal points at line " +
                        to_string(lineNumber) + ", column " + to_string(startCol));
                    i = temp;
                    column += (temp - i);
                    continue;
                }
                else {
                    tokens.push_back({ "NUMBER", number, lineNumber, startCol });
                    column += (temp - i);
                    i = temp;
                    lastTokenWasNumberOrIdentifier = true;
                    continue;
                }
            }

            // Else: not a number, treat '-' as operator
            tokens.push_back({ "OPERATOR", "-", lineNumber, startCol });
            i++; column++;
            continue;
        }

        if (isalpha(c) || c == '_') {
            int startCol = column;
            string lexeme;
            while (i < (int)line.size() && (isalnum(line[i]) || line[i] == '_')) {
                lexeme += line[i];
                i++; column++;
            }

            string type = isKeyword(lexeme) ? "KEYWORD" : "IDENTIFIER";
            tokens.push_back({ type, lexeme, lineNumber, startCol });

            // Check if the next token is '(' (function definition or call)
            if (i < (int)line.size() && line[i] == '(') {
                // Skip adding this identifier to the symbol table (it's a function)
                lastTokenWasNumberOrIdentifier = true;
                continue;
            }

            // check if we just saw def
            if (lexeme == "def") {
                seenDef = true;
            }
            else if (type == "IDENTIFIER" &&
                !seenDef &&
                find_if(symbolTable.begin(), symbolTable.end(),
                    [&lexeme](const Symbol& s) { return s.name == lexeme; }) == symbolTable.end()) {
                symbolTable.push_back(Symbol(lexeme, "UNKNOWN", ""));
            }

            lastTokenWasNumberOrIdentifier = true;
            continue;
        }
        // Detect invalid identifier like "1a"
        if (isdigit(c)) {
            int temp = i;
            string badId;
            while (temp < line.size() && (isalnum(line[temp]) || line[temp] == '_')) {
                badId += line[temp++];
            }
            if (badId.length() > 1 && isalpha(badId[1])) {
                errors.push_back("Invalid identifier: '" + badId + "' at line " +
                    to_string(lineNumber) + ", column " + to_string(column));
                // Skip the invalid identifier
                i = temp;
                column += (temp - i);
                continue;
            }
        }

        if (isdigit(c)) {
            int startCol = column;
            string number;
            bool isFloat = false;
            bool malformed = false;

            // Hexadecimal
            if (c == '0' && i + 1 < line.size() && (line[i + 1] == 'x' || line[i + 1] == 'X')) {
                number += line[i++]; column++; // '0'
                number += line[i++]; column++; // 'x' or 'X'
                while (i < line.size() && isalnum(line[i])) {  // Include Z or other invalids to validate later
                    number += line[i++]; column++;
                }
                if (!isValidHex(number)) {
                    errors.push_back("Invalid hexadecimal number '" + number + "' at line " + to_string(lineNumber) +
                        ", column " + to_string(startCol));
                }
                else {
                    tokens.push_back({ "NUMBER", number, lineNumber, startCol });
                }
                lastTokenWasNumberOrIdentifier = true;
                continue;
            }

            // Binary
            if (c == '0' && i + 1 < line.size() && (line[i + 1] == 'b' || line[i + 1] == 'B')) {
                number += line[i++]; column++;
                number += line[i++]; column++;
                while (i < line.size() && isalnum(line[i])) {
                    number += line[i++]; column++;
                }
                if (!isValidBinary(number)) {
                    errors.push_back("Invalid binary number '" + number + "' at line " + to_string(lineNumber) +
                        ", column " + to_string(startCol));
                }
                else {
                    tokens.push_back({ "NUMBER", number, lineNumber, startCol });
                }
                lastTokenWasNumberOrIdentifier = true;
                continue;
            }

            // Decimal or float
            while (i < line.size() && (isdigit(line[i]) || line[i] == '.')) {
                if (line[i] == '.') {
                    if (isFloat) {
                        errors.push_back("Invalid number format: multiple decimal points at line " +
                            to_string(lineNumber) + ", column " + to_string(startCol));
                        malformed = true;
                        break;
                    }
                    isFloat = true;
                }
                number += line[i++]; column++;
            }

            // Check for trailing garbage like 123abc
            if (i < line.size() && (isalpha(line[i]) || line[i] == '_')) {
                string invalid = number;
                while (i < line.size() && (isalnum(line[i]) || line[i] == '_')) {
                    invalid += line[i++]; column++;
                }
                errors.push_back("Malformed number '" + invalid + "' at line " + to_string(lineNumber) +
                    ", column " + to_string(startCol));
                continue;
            }

            if (!malformed)
                tokens.push_back({ "NUMBER", number, lineNumber, startCol });

            lastTokenWasNumberOrIdentifier = true;
            continue;
        }

        if (c == '"' || c == '\'') {
            char quote = c;
            int startCol = column;
            i++; column++;
            string strVal;
            bool closed = false;
            while (i < (int)line.size()) {
                if (line[i] == quote) {
                    closed = true;
                    i++; column++;
                    break;
                }
                strVal += line[i];
                i++; column++;
            }
            tokens.push_back({ "STRING", strVal, lineNumber, startCol });
            if (!closed) {
                string quoteType = (quote == '"') ? "double" : "single";
                errors.push_back("Unclosed " + quoteType + " quote at line " + to_string(lineNumber) +
                    ", column " + to_string(startCol));
            }

            lastTokenWasNumberOrIdentifier = true;
            continue;
        }

        int startCol = column;
        string op(1, c);
        Token token = { "", op, lineNumber, startCol };

        // Handle multi-char operators and check for invalid sequences
        if (string("=<>!+-*/%").find(c) != string::npos) {
            int startCol = column;
            string op;
            int tempI = i;

            // Build up contiguous operator sequence
            while (tempI < line.size() && string("=<>!+-*/%").find(line[tempI]) != string::npos) {
                op += line[tempI++];
            }

            // Known valid operators
            static const vector<string> validOperators = {
                "+", "-", "", "/", "%", "=", "==", "!=", ">=", "<=", "<", ">", "+=", "-=", "=", "/=", "%="
            };

            // Check if whole sequence is valid
            if (find(validOperators.begin(), validOperators.end(), op) == validOperators.end()) {
                errors.push_back("Invalid operator sequence '" + op + "' at line " +
                    to_string(lineNumber) + ", column " + to_string(column));
                i = tempI;
                column += op.length();
                continue;
            }

            tokens.push_back({ "OPERATOR", op, lineNumber, startCol });
            i = tempI;
            column += op.length();
            lastTokenWasNumberOrIdentifier = false;
            continue;
        }

        // Stack-based delimiter matching with specific token names
        if (c == '(') {
            tokens.push_back({ "LParenthese", "(", lineNumber, column });
            delimiterStack.push({ c, column });
            i++; column++;
            continue;
        }
        else if (c == ')') {
            if (delimiterStack.empty()) {
                errors.push_back("Unmatched closing ')' at line " +
                    to_string(lineNumber) + ", column " + to_string(column));
            }
            else {
                char expectedOpen = delimiterStack.top().first;
                if (expectedOpen == '(') {
                    tokens.push_back({ "RParenthese", ")", lineNumber, column });
                    delimiterStack.pop();
                }
                else {
                    errors.push_back("Mismatched closing ')' at line " +
                        to_string(lineNumber) + ", column " + to_string(column));
                }
            }
            i++; column++;
            continue;
        }
        else if (c == '[') {
            tokens.push_back({ "LSquarebracket", "[", lineNumber, column });
            delimiterStack.push({ c, column });
            i++; column++;
            continue;
        }
        else if (c == ']') {
            if (delimiterStack.empty()) {
                errors.push_back("Unmatched closing ']' at line " +
                    to_string(lineNumber) + ", column " + to_string(column));
            }
            else {
                char expectedOpen = delimiterStack.top().first;
                if (expectedOpen == '[') {
                    tokens.push_back({ "RSquarebracket", "]", lineNumber, column });
                    delimiterStack.pop();
                }
                else {
                    errors.push_back("Mismatched closing ']' at line " +
                        to_string(lineNumber) + ", column " + to_string(column));
                }
            }
            i++; column++;
            continue;
        }
        else if (c == '{') {
            tokens.push_back({ "LCurlybrace", "{", lineNumber, column });
            delimiterStack.push({ c, column });
            i++; column++;
            continue;
        }
        else if (c == '}') {
            if (delimiterStack.empty()) {
                errors.push_back("Unmatched closing '}' at line " +
                    to_string(lineNumber) + ", column " + to_string(column));
            }
            else {
                char expectedOpen = delimiterStack.top().first;
                if (expectedOpen == '{') {
                    tokens.push_back({ "RCurlybrace", "}", lineNumber, column });
                    delimiterStack.pop();
                }
                else {
                    errors.push_back("Mismatched closing '}' at line " +
                        to_string(lineNumber) + ", column " + to_string(column));
                }
            }
            i++; column++;
            continue;
        }
        else if (c == '.') {
            tokens.push_back({ "FullStop", ".", lineNumber, column });
            i++; column++;
            continue;
        }
        else if (c == ';') {
            tokens.push_back({ "Semicolon", ";", lineNumber, column });
            i++; column++;
            continue;
        }
        else {
            errors.push_back("Unknown character '" + string(1, c) +
                "' at line " + to_string(lineNumber) +
                ", column " + to_string(column));
        }

        if (!token.type.empty())
            tokens.push_back(token);

        lastTokenWasNumberOrIdentifier = false;
        i++; column++;
    }
    while (!delimiterStack.empty()) {
        auto [open, col] = delimiterStack.top();
        delimiterStack.pop();
        errors.push_back(string("Unmatched opening '") + open + "' at line " +
            to_string(lineNumber) + ", column " + to_string(col));
    }
}

// Precedence & helpers
int precedence(const string& op) {
    if (op == "+" || op == "-") return 1;
    if (op == "*" || op == "/") return 2;
    return 0;
}

vector<Token> infixToPostfix(const vector<Token>& infix) {
    vector<Token> output;
    stack<Token> opStack;
    for (const auto& token : infix) {
        if (token.type == "NUMBER" || token.type == "IDENTIFIER")
            output.push_back(token);
        else if (token.type == "OPERATOR") {
            while (!opStack.empty() && precedence(opStack.top().value) >= precedence(token.value)) {
                output.push_back(opStack.top()); opStack.pop();
            }
            opStack.push(token);
        }
    }
    while (!opStack.empty()) {
        output.push_back(opStack.top());
        opStack.pop();
    }
    return output;
}

bool evaluatePostfix(const vector<Token>& postfix, const vector<Symbol>& symbolTable, string& resultValue) {
    stack<double> evalStack;
    for (const Token& token : postfix) {
        if (token.type == "NUMBER") {
            const string& val = token.value;
            try {
                if (val.rfind("0x", 0) == 0 || val.rfind("0X", 0) == 0) {
                    evalStack.push(static_cast<double>(stoi(val, nullptr, 16))); // Hex
                }
                else if (val.rfind("0b", 0) == 0 || val.rfind("0B", 0) == 0) {
                    evalStack.push(static_cast<double>(stoi(val.substr(2), nullptr, 2))); // Binary
                }
                else if (val.find('.') != string::npos) {
                    evalStack.push(stod(val));  // Float
                }
                else {
                    evalStack.push(stoi(val));  // Decimal int
                }
            }
            catch (...) {
                return false; // Malformed number
            }
        }

        else if (token.type == "IDENTIFIER") {
            auto it = find_if(symbolTable.begin(), symbolTable.end(),
                [&](const Symbol& s) { return s.name == token.value; });
            if (it == symbolTable.end() || it->value.empty()) return false;
            evalStack.push(stod(it->value));
        }
        else if (token.type == "OPERATOR") {
            if (evalStack.size() < 2) return false;
            double b = evalStack.top(); evalStack.pop();
            double a = evalStack.top(); evalStack.pop();
            if (token.value == "+") evalStack.push(a + b);
            else if (token.value == "-") evalStack.push(a - b);
            else if (token.value == "*") evalStack.push(a * b);
            else if (token.value == "/") {
                if (b == 0) return false;
                evalStack.push(a / b);
            }
        }
    }

    if (evalStack.size() != 1) return false;
    double result = evalStack.top();
    ostringstream oss;
    oss << result;
    resultValue = oss.str();

    if (resultValue.find('.') != string::npos) {
        resultValue.erase(resultValue.find_last_not_of('0') + 1);
        if (resultValue.back() == '.') resultValue.pop_back();
    }

    return true;
}

// Symbol table builder
void buildSymbolTableFromTokens(const vector<Token>& tokens, vector<Symbol>& symbolTable) {
    unordered_map<string, vector<Token>> pendingExpressions;

    for (size_t i = 0; i < tokens.size(); ++i) {
        if (i + 2 < tokens.size() &&
            tokens[i].type == "IDENTIFIER" && // Check if it's an identifier (could be a function or variable)
            tokens[i + 1].type == "OPERATOR" && tokens[i + 1].value == "=") {

            string varName = tokens[i].value;
            size_t j = i + 2;
            vector<Token> expr;

            // STRING assignment
            if (tokens[j].type == "STRING") {
                string value = tokens[j].value;
                string type = "STRING";

                auto it = find_if(symbolTable.begin(), symbolTable.end(),
                    [&](const Symbol& s) { return s.name == varName; });
                if (it == symbolTable.end())
                    symbolTable.push_back(Symbol(varName, type, value));
                else { it->type = type; it->value = value; }

                i = j;
                continue;
            }

            // Collect arithmetic expression tokens
            while (j < tokens.size() &&
                (tokens[j].type == "IDENTIFIER" || tokens[j].type == "NUMBER" ||
                    tokens[j].type == "OPERATOR" || tokens[j].type == "LPAREN" || tokens[j].type == "RPAREN")) {
                expr.push_back(tokens[j]);
                j++;
            }

            // Attempt to evaluate now
            string resultValue, type = "INTEGER";
            bool evaluated = evaluatePostfix(infixToPostfix(expr), symbolTable, resultValue);
            if (!evaluated) {
                type = "UNKNOWN";
                resultValue = "";
                pendingExpressions[varName] = expr;  // Save for later
            }
            else if (resultValue.find('.') != string::npos) {
                type = "FLOAT";
            }

            auto it = find_if(symbolTable.begin(), symbolTable.end(),
                [&](const Symbol& s) { return s.name == varName; });
            if (it == symbolTable.end())
                symbolTable.push_back(Symbol(varName, type, resultValue));
            else { it->type = type; it->value = resultValue; }

            for (const auto& tok : expr) {
                if (tok.type == "IDENTIFIER") {
                    // Avoid inserting 0x123 or 0b1010 thinking they're identifiers
                    if ((tok.value.rfind("0x", 0) == 0 || tok.value.rfind("0X", 0) == 0) ||
                        (tok.value.rfind("0b", 0) == 0 || tok.value.rfind("0B", 0) == 0)) {
                        continue; // it's a number, not an identifier
                    }

                    auto ex = find_if(symbolTable.begin(), symbolTable.end(),
                        [&](const Symbol& s) { return s.name == tok.value; });
                    if (ex == symbolTable.end())
                        symbolTable.push_back(Symbol(tok.value, "UNKNOWN", ""));
                }
            }

            i = j - 1;
        }
    }

    // Second pass: re-evaluate previously unresolved expressions
    for (auto& pair : pendingExpressions) {
        const string& varName = pair.first;
        const vector<Token>& expr = pair.second;

        string resultValue, type = "INTEGER";
        if (evaluatePostfix(infixToPostfix(expr), symbolTable, resultValue)) {
            if (resultValue.find('.') != string::npos)
                type = "FLOAT";

            auto it = find_if(symbolTable.begin(), symbolTable.end(),
                [&](const Symbol& s) { return s.name == varName; });
            if (it != symbolTable.end()) {
                it->type = type;
                it->value = resultValue;
            }
        }
    }
}

// Displays tokens, symbol table, and lexical errors in the GUI.
void displayResults(const vector<Token>& tokens,
    const vector<Symbol>& symbolTable,
    const vector<string>& errors)
{
    // Displaying the tokens (excluding INDENT, DEDENT, NEWLINE, EOF)
    ostringstream ss; // Using 'ss' as per your original variable name
    ss << "Tokens:\r\n";

    for (const auto& tok : tokens) { // Changed to const auto&, good practice
        // <<< --- THIS IS THE MODIFICATION --- >>>
        if (tok.type == "INDENT" ||
            tok.type == "DEDENT" ||
            (tok.type == "NEWLINE" && tok.value == "\n") || // Only skip "pure" standard newlines
            tok.type == "EOF") {
            continue; // Skip displaying these specific token types
        }
        // <<< --- END OF MODIFICATION --- >>>


        std::string displayValue = tok.value;
        if (tok.type == "NEWLINE") { // In case you decide to show some NEWLINEs but make them visible
            displayValue = "\\n";
        }


        ss << "[" << tok.line << ":" << tok.column << "] "
            << tok.type << " -> " << displayValue << "\r\n"; // Used displayValue
    }
    // Optional: Add a closing separator
    // ss << "--------------------------------------------------\r\n";
    SetWindowTextA(GetDlgItem(GetActiveWindow(), IDC_OUTPUT), ss.str().c_str());

    // Displaying the symbol table (remains the same as your code)
    ostringstream ssSym;
    ssSym << "Symbol Table:\r\n";
    ssSym << "Index | Name   | Type     | Value\r\n";
    ssSym << "-------------------------------\r\n";
    for (size_t i = 0; i < symbolTable.size(); ++i) {
        const Symbol& sym = symbolTable[i];
        // Optional: Truncate symbol value if it can be very long
        // std::string symDisplayValue = sym.value;
        // if (symDisplayValue.length() > 20) {
        //     symDisplayValue = symDisplayValue.substr(0, 17) + "...";
        // }
        // ssSym << i + 1 << "     | " << sym.name << "    | " << sym.type << "    | " << symDisplayValue << "\r\n";
        ssSym << i + 1 << "     | " << sym.name << "    | " << sym.type << "    | " << sym.value << "\r\n";
    }
    SetWindowTextA(GetDlgItem(GetActiveWindow(), IDC_SYMBOL), ssSym.str().c_str());

    // Displaying lexical errors (remains the same as your code)
    ostringstream ssErr;
    ssErr << "Lexical Errors:\r\n";
    if (errors.empty()){
        ssErr << "(No lexical errors detected)\r\n";
    } else {
        for (const auto& err : errors) { // Changed to const auto&
            ssErr << err << "\r\n";
        }
    }
    SetWindowTextA(GetDlgItem(GetActiveWindow(), IDC_ERRORS), ssErr.str().c_str());
}
// =================================================================================
// PARSER AND GUI CODE SECTION
// =================================================================================

// --- Structure for Concrete Syntax Tree (Parse Tree) Node ---
struct ParseTreeNode {
    std::string name;
    std::vector<ParseTreeNode*> children;

    ParseTreeNode(const std::string& n) : name(n) {}
    ~ParseTreeNode() {
        for (ParseTreeNode* child : children) {
            delete child;
        }
    }
    ParseTreeNode(const ParseTreeNode&) = delete;
    ParseTreeNode& operator=(const ParseTreeNode&) = delete;
    ParseTreeNode(ParseTreeNode&& other) noexcept : name(std::move(other.name)), children(std::move(other.children)) {
        other.children.clear();
    }
    ParseTreeNode& operator=(ParseTreeNode&& other) noexcept {
        if (this != &other) {
            for (ParseTreeNode* child : children) { delete child; }
            name = std::move(other.name);
            children = std::move(other.children);
            other.children.clear();
        }
        return *this;
    }
    void addChild(ParseTreeNode* child) {
        if (child) {
            children.push_back(child);
        }
    }
};

// --- Parser Class Definition (for CST) ---
class Parser {
public:
    Parser(const std::vector<Token>& tokens, std::vector<std::string>& errors)
        : tokens_(tokens), errors_(errors), currentTokenIndex_(0) {}

    ParseTreeNode* parseProgram() { // Public entry point
         return parseProgram_impl();
    }
    const std::vector<std::string>& getErrors() const {
        return errors_;
    }

private:
    const std::vector<Token>& tokens_;
    std::vector<std::string>& errors_;
    size_t currentTokenIndex_;

    // Helper Methods
    const Token& currentToken(size_t offset = 0) const;
    const Token& previousToken() const;
    const Token& lookaheadSignificant(size_t count = 1) const;
    bool isAtEnd(bool countEofAsEnd = true) const;
    void advance();
    const Token& currentSignificantToken();
    bool match(const std::string& expectedType, const std::string& expectedValue = "");
    bool matchExact(const std::string& type, const std::string& value = "");
    bool consumeSignificant(const std::string& expectedType, const std::string& expectedValue = "", Token* consumedToken = nullptr);
    bool consumeExact(const std::string& expectedType, const std::string& expectedValue = "", Token* consumedToken = nullptr);
    void error(const std::string& message);
    void addTerminalNode(ParseTreeNode* parent, const Token& token, const std::string& ruleName = "");
    void addEpsilonNode(ParseTreeNode* parent);
    bool isAssignmentOperator(const Token& token) const;
    bool canStartStatement(const Token& token) const;
    bool isCompOperator(const Token& token) const {
        if (token.type != "OPERATOR") return false;
        static const std::string validOps[] = {"==", "!=", "<=", ">=", "<", ">"};
        for (const auto& op : validOps) {
            if (token.value == op) return true;
        }
        return false;
    }

    // Recursive Descent Parsing Methods
    ParseTreeNode* parseProgram_impl();
    ParseTreeNode* parseStmtList();
    ParseTreeNode* parseStmtListTail();
    ParseTreeNode* parseStmt();
    ParseTreeNode* parseAssignLikeStmt();
    ParseTreeNode* parseAssignOp();
    ParseTreeNode* parseIfStmt();
    ParseTreeNode* parseElseClause();
    ParseTreeNode* parseWhileStmt();
    ParseTreeNode* parseForStmt();
    ParseTreeNode* parseFuncDef();
    ParseTreeNode* parseBlock();
    ParseTreeNode* parseParamList();
    ParseTreeNode* parseParamTail();
    ParseTreeNode* parsePrintStmt();
    ParseTreeNode* parseReturnStmt();
    ParseTreeNode* parseExprStmt();
    ParseTreeNode* parseExpr();
    ParseTreeNode* parseLogicExpr();
    ParseTreeNode* parseLogicTail();
    ParseTreeNode* parseLogicOp();
    ParseTreeNode* parseCompExpr();
    ParseTreeNode* parseCompTail();
    ParseTreeNode* parseCompOp();
    ParseTreeNode* parseArithExpr();
    ParseTreeNode* parseAddTail();
    ParseTreeNode* parseAddOp();
    ParseTreeNode* parseTerm();
    ParseTreeNode* parseMulTail();
    ParseTreeNode* parseMulOp();
    ParseTreeNode* parseFactor();
    ParseTreeNode* parseFuncCall();
    ParseTreeNode* parseArgList();
    ParseTreeNode* parseArgTail();
};
// --- Parser Method Implementations ---
const Token& Parser::currentToken(size_t offset) const {
    if (tokens_.empty()) { static Token dummyEOF = {"EOF", "", 0, 0}; return dummyEOF; }
    if (currentTokenIndex_ + offset >= tokens_.size()) return tokens_.back();
    return tokens_[currentTokenIndex_ + offset];
}

const Token& Parser::previousToken() const {
     if (currentTokenIndex_ == 0 || tokens_.empty()) {
        static Token dummyErrorToken = {"ERROR_TOKEN", "PREVIOUS_OUT_OF_BOUNDS", 0,0};
        return dummyErrorToken;
    }
    return tokens_[currentTokenIndex_ -1];
}

const Token& Parser::lookaheadSignificant(size_t count) const {
    if (tokens_.empty()) { static Token dummyEOF = {"EOF", "", 0, 0}; return dummyEOF; }
    if (count == 0) {
        size_t tempIndex = currentTokenIndex_;
        while(tempIndex < tokens_.size() && tokens_[tempIndex].type == "NEWLINE") {
            if (tempIndex == tokens_.size() - 1 && tokens_[tempIndex].type == "NEWLINE") break;
            tempIndex++;
        }
        if (tempIndex >= tokens_.size()) return tokens_.back();
        return tokens_[tempIndex];
    }
    size_t searchStartIndex = currentTokenIndex_;
    while(searchStartIndex < tokens_.size() && tokens_[searchStartIndex].type == "NEWLINE") {
         if (searchStartIndex == tokens_.size() - 1 && tokens_[searchStartIndex].type == "NEWLINE") break;
        searchStartIndex++;
    }
    if (searchStartIndex >= tokens_.size()) return tokens_.back();
    size_t significantFound = 0;
    size_t effectiveIndex = searchStartIndex;
    while (effectiveIndex < tokens_.size() -1 ) {
        effectiveIndex++;
        if (tokens_[effectiveIndex].type != "NEWLINE") {
            significantFound++;
            if (significantFound == count) {
                return tokens_[effectiveIndex];
            }
        }
    }
    return tokens_.back();
}

bool Parser::isAtEnd(bool countEofAsEnd) const {
    if (tokens_.empty()) return true;
    if (currentTokenIndex_ >= tokens_.size()) return true;
    if (countEofAsEnd && tokens_[currentTokenIndex_].type == "EOF") return true;
    if (!countEofAsEnd && currentTokenIndex_ == tokens_.size() -1 && tokens_[currentTokenIndex_].type == "EOF") return true;
    return false;
}

void Parser::advance() {
    if (currentTokenIndex_ < tokens_.size()) {
        currentTokenIndex_++;
    }
}

const Token& Parser::currentSignificantToken() {
    if (tokens_.empty()) { static Token dummyEOF = {"EOF", "", 0, 0}; return dummyEOF; }
    while(currentTokenIndex_ < tokens_.size() -1 && tokens_[currentTokenIndex_].type == "NEWLINE") {
        advance();
    }
    // After skipping newlines, currentTokenIndex_ points to the significant token OR EOF OR is past end
    if (currentTokenIndex_ >= tokens_.size()) return tokens_.back(); // Should be EOF if vector not empty
    return tokens_[currentTokenIndex_];
}

bool Parser::match(const std::string& expectedType, const std::string& expectedValue) {
    size_t savedIndex = currentTokenIndex_;
    const Token& sigToken = currentSignificantToken(); // This advances currentTokenIndex_
    bool result = false;
    if (sigToken.type == "EOF" && expectedType != "EOF") {
         currentTokenIndex_ = savedIndex; // Restore before returning
         return false;
    }
    if (sigToken.type == expectedType) {
        if (expectedValue.empty() || sigToken.value == expectedValue) {
            result = true;
        }
    }
    currentTokenIndex_ = savedIndex; // Crucially restore index, match is a peek
    return result;
}

bool Parser::matchExact(const std::string& type, const std::string& value) {
    const Token& tok = currentToken(); // Does not skip newlines
    if (tok.type == "EOF") return (type == "EOF");
    if (tok.type == type) {
        return value.empty() || tok.value == value;
    }
    return false;
}

bool Parser::consumeSignificant(const std::string& expectedType, const std::string& expectedValue, Token* consumedToken) {
    size_t initialIndex = currentTokenIndex_; // Save index before currentSignificantToken modifies it
    const Token& sigToken = currentSignificantToken(); // Advances currentTokenIndex_ to the significant token or EOF

    if (sigToken.type == "EOF" && expectedType != "EOF") {
        error("Expected token type '" + expectedType + "'" +
              (expectedValue.empty() ? "" : " with value '" + expectedValue + "'") +
              ", but found EOF at line " + std::to_string(sigToken.line) +
              ", col " + std::to_string(sigToken.column));
        currentTokenIndex_ = initialIndex; // Restore original index, no consumption
        return false;
    }

    if (sigToken.type == expectedType) {
        if (expectedValue.empty() || sigToken.value == expectedValue) {
            if (consumedToken) *consumedToken = sigToken;
            advance(); // Advance past the significant token that was just processed
            return true;
        }
    }

    error("Expected token type '" + expectedType + "'" +
          (expectedValue.empty() ? "" : " with value '" + expectedValue + "'") +
          ", but got type '" + sigToken.type + "'" +
          (sigToken.value.empty() ? "" : " with value '" + sigToken.value + "'") +
          " at line " + std::to_string(sigToken.line) +
          ", col " + std::to_string(sigToken.column));
    currentTokenIndex_ = initialIndex; // Restore original index, no consumption
    return false;
}

bool Parser::consumeExact(const std::string& expectedType, const std::string& expectedValue, Token* consumedToken) {
    if (matchExact(expectedType, expectedValue)) { // matchExact does not change currentTokenIndex_
        if(consumedToken) *consumedToken = currentToken();
        advance(); // Consume the matched token
        return true;
    }
    if (!(expectedType == "EOF" && (isAtEnd(true) || currentToken().type == "EOF" ))) {
        const Token& cur = currentToken(); // Token that caused mismatch
        error("Expected token type '" + expectedType + "'" +
            (expectedValue.empty() ? "" : " with value '" + expectedValue + "'") +
            ", but got type '" + cur.type + "'" +
            (cur.value.empty() ? "" : " with value '" + cur.value + "'") +
            " at line " + std::to_string(cur.line) +
            ", col " + std::to_string(cur.column));
    }
    return false;
}
void Parser::error(const std::string& message) {
    if (!errors_.empty()) {
        const auto& lastError = errors_.back();
        const Token& curTokForError = currentToken();
         if (lastError.find(message.substr(0, std::min(message.size(), (size_t)20))) != std::string::npos &&
            lastError.find("line " + std::to_string(curTokForError.line)) != std::string::npos &&
            lastError.find("col " + std::to_string(curTokForError.column)) != std::string::npos) {
            // return; // Suppress
        }
    }
    errors_.push_back(message);
}

void Parser::addTerminalNode(ParseTreeNode* parent, const Token& token, const std::string& ruleName) {
    if (!parent) return;
    std::string label;
    if (!ruleName.empty()) {
        label = ruleName;
        if ((ruleName == "id" || ruleName == "num" || ruleName == "string") && !token.value.empty()) {
             label += " (" + token.value + ")";
        } else if (ruleName == "$ (EOF)") {
            label = "$ (EOF)";
        }
    } else {
        if (token.type == "KEYWORD" || token.type == "OPERATOR" || token.type == "Colon" || token.type == "Comma" ||
            token.type == "LParenthese" || token.type == "RParenthese" ||
            token.type == "LSquarebracket" || token.type == "RSquarebracket" ||
            token.type == "LCurlybrace" || token.type == "RCurlybrace" ||
            token.type == "FullStop" || token.type == "Semicolon") {
            label = "'" + token.value + "'";
        } else if (token.type == "IDENTIFIER" || token.type == "NUMBER" || token.type == "STRING") {
            label = token.type + ": " + token.value;
        } else { label = token.type; }
    }
    parent->addChild(new ParseTreeNode(label));
}

void Parser::addEpsilonNode(ParseTreeNode* parent) {
    if (parent) parent->addChild(new ParseTreeNode("ε"));
}

bool Parser::isAssignmentOperator(const Token& token) const {
    if (token.type != "OPERATOR") return false;
    const std::string ops[] = {"=", "+=", "-=", "*=", "/=", "%="};
    for (const auto& op : ops) {
        if (token.value == op) return true;
    }
    return false;
}

bool Parser::canStartStatement(const Token& token) const {
    if (token.type == "EOF" || token.type == "DEDENT") return false;
    if (token.type == "IDENTIFIER") return true;
    if (token.type == "KEYWORD") {
        const auto& v = token.value;
        return v == "if" || v == "while" || v == "for" || v == "def" ||
               v == "print" || v == "return" || v == "pass" ;
    }
    if (token.type == "NUMBER" || token.type == "STRING" || token.type == "LParenthese" ||
        (token.type == "KEYWORD" && (token.value == "True" || token.value == "False" || token.value == "None"))) return true;
    return false;
}

ParseTreeNode* Parser::parseProgram_impl() {
    ParseTreeNode* ptn = new ParseTreeNode("<program>");
    size_t initialIndex = currentTokenIndex_;

    // Skip leading newlines
    while (currentTokenIndex_ < tokens_.size() && tokens_[currentTokenIndex_].type == "NEWLINE") {
        advance();
    }

    ParseTreeNode* stmtListNode = parseStmtList();
    if (stmtListNode) {
        ptn->addChild(stmtListNode);
    } else {
        if (errors_.empty() && (currentToken().type == "EOF" || currentTokenIndex_ == initialIndex)) {
            ParseTreeNode* emptySlNode = new ParseTreeNode("<stmt_list>");
            addEpsilonNode(emptySlNode);
            ptn->addChild(emptySlNode);
        }
    }

    // Skip trailing newlines before checking for EOF
    while (currentTokenIndex_ < tokens_.size() && tokens_[currentTokenIndex_].type == "NEWLINE") {
        advance();
    }

    // Check for trailing tokens
    if (currentTokenIndex_ < tokens_.size() && tokens_[currentTokenIndex_].type != "EOF") {
        const Token& trailingToken = tokens_[currentTokenIndex_];
        error("Unexpected token '" + trailingToken.value + "' (type: " + trailingToken.type +
              ") after program statements at line " + std::to_string(trailingToken.line) +
              ", col " + std::to_string(trailingToken.column) + ". Expected end of file.");
    }

    return ptn;
}

ParseTreeNode* Parser::parseStmtList() {
    size_t listStartIndex = currentTokenIndex_;
    while(matchExact("NEWLINE")) advance();

    size_t peekIndex = currentTokenIndex_;
    while(peekIndex < tokens_.size() && tokens_[peekIndex].type == "NEWLINE"){
        if(peekIndex == tokens_.size() -1) break; // Avoid going past end if last is newline
        peekIndex++;
    }
    const Token& firstSignificantInList = (peekIndex < tokens_.size()) ? tokens_[peekIndex] : tokens_.back();

    if (firstSignificantInList.type == "DEDENT" || firstSignificantInList.type == "EOF" || !canStartStatement(firstSignificantInList)) {
         currentTokenIndex_ = listStartIndex;
         ParseTreeNode* ptn_eps = new ParseTreeNode("<stmt_list>");
         addEpsilonNode(ptn_eps);
         return ptn_eps;
    }

    ParseTreeNode* ptn = new ParseTreeNode("<stmt_list>");
    ParseTreeNode* stmtNode = parseStmt();
    if (stmtNode) {
        ptn->addChild(stmtNode);
        ParseTreeNode* tailNode = parseStmtListTail();
        if (tailNode) {
            ptn->addChild(tailNode);
            return ptn;
        } else { delete ptn; currentTokenIndex_ = listStartIndex; return nullptr; }
    } else {
        delete ptn;
        currentTokenIndex_ = listStartIndex;
        return nullptr;
    }
}

ParseTreeNode* Parser::parseStmtListTail() {
    ParseTreeNode* ptn = new ParseTreeNode("<stmt_list_tail>");
    size_t tailStartIndex = currentTokenIndex_;

    size_t indexBeforeNewlines = currentTokenIndex_;
    while(matchExact("NEWLINE")) {
        advance();
    }

    size_t peekIndex = currentTokenIndex_;
    while(peekIndex < tokens_.size() && tokens_[peekIndex].type == "NEWLINE"){
        if(peekIndex == tokens_.size() -1) break;
        peekIndex++;
    }
    const Token& nextSignificantForTail = (peekIndex < tokens_.size()) ? tokens_[peekIndex] : tokens_.back();

    if (nextSignificantForTail.type == "DEDENT" || nextSignificantForTail.type == "EOF" || !canStartStatement(nextSignificantForTail)) {
        currentTokenIndex_ = tailStartIndex;
        addEpsilonNode(ptn);
    } else {
        ParseTreeNode* stmtNode = parseStmt();
        if (stmtNode) {
            ptn->addChild(stmtNode);
            ParseTreeNode* recursiveTailNode = parseStmtListTail();
            if (recursiveTailNode) {
                ptn->addChild(recursiveTailNode);
            } else { delete ptn; currentTokenIndex_ = tailStartIndex; return nullptr; }
        } else {
            currentTokenIndex_ = indexBeforeNewlines;
            addEpsilonNode(ptn);
        }
    }
    return ptn;
}

ParseTreeNode* Parser::parseStmt() {
    ParseTreeNode* stmtNodeRoot = new ParseTreeNode("<stmt>");
    ParseTreeNode* specificStmtProduction = nullptr;
    size_t stmtInitialIndex = currentTokenIndex_; // Index before any operations for this <stmt>
    std::vector<std::string> stmtInitialErrors = errors_;

    // Determine the first significant token for this statement.
    const Token& firstSigTok = currentSignificantToken(); // This advances currentTokenIndex_
    size_t afterSkippingNewlinesIndex = currentTokenIndex_ ; // This is actually index after firstSigTok now

    // For parsing functions to work correctly, they need to start at firstSigTok.
    // So we need the index of firstSigTok itself.
    size_t indexOfFirstSigTok = afterSkippingNewlinesIndex > 0 ? afterSkippingNewlinesIndex -1 : 0;
    if (tokens_.empty() || indexOfFirstSigTok >= tokens_.size() ) { // Safety for empty tokens
        indexOfFirstSigTok = (tokens_.empty() ? 0 : tokens_.size() -1);
    } else if (firstSigTok.type == "NEWLINE" && afterSkippingNewlinesIndex > 0) {
         // If currentSignificantToken landed on a NEWLINE (e.g. end of input only has newlines)
         // Then firstSigTok IS that newline, so its index is currentTokenIndex_ - 1.
         // This case is less common for statement parsing.
    } else if (afterSkippingNewlinesIndex == stmtInitialIndex && stmtInitialIndex < tokens_.size()){
        indexOfFirstSigTok = stmtInitialIndex; // No newlines were skipped
    } // Otherwise indexOfFirstSigTok is correctly afterSkippingNewlinesIndex - 1

    if (indexOfFirstSigTok >= tokens_.size() && !tokens_.empty()) indexOfFirstSigTok = tokens_.size()-1; // Bounds guard

    // 1. Try <assign_like_stmt>
    if (firstSigTok.type == "IDENTIFIER") {
        const Token& nextSigTok = lookaheadSignificant(1); // Peek at the token after firstSigTok
        if (isAssignmentOperator(nextSigTok)) {
            currentTokenIndex_ = indexOfFirstSigTok; // Position parser AT the IDENTIFIER
            specificStmtProduction = parseAssignLikeStmt();
            if (specificStmtProduction) {
                stmtNodeRoot->addChild(specificStmtProduction);
                return stmtNodeRoot;
            }
            currentTokenIndex_ = stmtInitialIndex; errors_ = stmtInitialErrors; // Full backtrack
        }
    }

    // 2. Try keyword-based statements
    currentTokenIndex_ = indexOfFirstSigTok; // Position parser AT firstSigTok for these attempts
    if (firstSigTok.type == "KEYWORD") {
        if (firstSigTok.value == "if") specificStmtProduction = parseIfStmt();
        else if (firstSigTok.value == "while") specificStmtProduction = parseWhileStmt();
        else if (firstSigTok.value == "for") specificStmtProduction = parseForStmt();
        else if (firstSigTok.value == "def") specificStmtProduction = parseFuncDef();
        else if (firstSigTok.value == "print") specificStmtProduction = parsePrintStmt();
        else if (firstSigTok.value == "return") specificStmtProduction = parseReturnStmt();

        if (specificStmtProduction) {
            stmtNodeRoot->addChild(specificStmtProduction);
            return stmtNodeRoot;
        }
        currentTokenIndex_ = stmtInitialIndex; errors_ = stmtInitialErrors; // Full backtrack
    }

    // 3. Try <expr_stmt> as a fallback
    currentTokenIndex_ = indexOfFirstSigTok; // Position parser AT firstSigTok
    if (canStartStatement(firstSigTok)) {
        specificStmtProduction = parseExprStmt();
        if (specificStmtProduction) {
            stmtNodeRoot->addChild(specificStmtProduction);
            return stmtNodeRoot;
        }
        currentTokenIndex_ = stmtInitialIndex; errors_ = stmtInitialErrors; // Full backtrack
    }

    // If no statement rule matched:
    delete stmtNodeRoot;
    if (firstSigTok.type != "DEDENT" && firstSigTok.type != "EOF" && firstSigTok.type != "NEWLINE") {
        bool errorExistsForPos = false;
        if (errors_.size() > stmtInitialErrors.size()) {
            for(size_t errIdx = stmtInitialErrors.size(); errIdx < errors_.size(); ++errIdx) {
                if (errors_[errIdx].find("line " + std::to_string(firstSigTok.line)) != std::string::npos &&
                    errors_[errIdx].find("col " + std::to_string(firstSigTok.column)) != std::string::npos) {
                    errorExistsForPos = true; break;
                }
            }
        }
        if(!errorExistsForPos) {
             error("Syntax Error: Unexpected token '" + firstSigTok.value + "' (type: " + firstSigTok.type + ") when expecting a statement at line " + std::to_string(firstSigTok.line) + ", col " + std::to_string(firstSigTok.column));
        }
    }
    currentTokenIndex_ = stmtInitialIndex;
    return nullptr;
}

ParseTreeNode* Parser::parseAssignLikeStmt() {
    ParseTreeNode* ptn = new ParseTreeNode("<assign_like_stmt>");
    size_t ruleStartIndex = currentTokenIndex_;
    std::vector<std::string> ruleStartErrors = errors_;
    Token idTok;

    if (consumeSignificant("IDENTIFIER", "", &idTok)) {
        ParseTreeNode* assignOpNode = parseAssignOp();
        if (assignOpNode) {
            ParseTreeNode* exprNode = parseExpr();
            if (exprNode) {
                addTerminalNode(ptn, idTok, "id");
                ptn->addChild(assignOpNode);
                ptn->addChild(exprNode);
                return ptn;
            }
            delete assignOpNode;
        }
    }
    currentTokenIndex_ = ruleStartIndex;
    errors_ = ruleStartErrors;
    delete ptn;
    return nullptr;
}

ParseTreeNode* Parser::parseAssignOp() {
    ParseTreeNode* ptn = new ParseTreeNode("<assign_op>");
    Token opTok;
    const std::string valid_ops[] = {"=", "+=", "-=", "*=", "/=", "%="};
    for (const auto& op_val : valid_ops) {
        if (match("OPERATOR", op_val)) {
             if (consumeSignificant("OPERATOR", op_val, &opTok)) {
                addTerminalNode(ptn, opTok);
                return ptn;
             } else { delete ptn; return nullptr;}
        }
    }
    delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseIfStmt() {
    ParseTreeNode* ptn = new ParseTreeNode("<if_stmt>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    Token ifTok, colonTok;

    if (consumeSignificant("KEYWORD", "if", &ifTok)) {
        addTerminalNode(ptn, ifTok);
        ParseTreeNode* exprNode = parseExpr();
        if (exprNode) {
            ptn->addChild(exprNode);
            if (consumeSignificant("Colon", ":", &colonTok)) {
                addTerminalNode(ptn, colonTok);
                ParseTreeNode* blockNode = parseBlock();
                if (blockNode) {
                    ptn->addChild(blockNode);
                    ParseTreeNode* elseClauseNode = parseElseClause();
                    if(elseClauseNode) {ptn->addChild(elseClauseNode); return ptn; }
                     delete ptn; currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; return nullptr;

                }
            }
        }
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseElseClause() {
    ParseTreeNode* ptn = new ParseTreeNode("<else_clause>");
    Token elseTok, colonTok;

    if (match("KEYWORD", "else")) {
        size_t elseAttemptIndex = currentTokenIndex_; std::vector<std::string> elseAttemptErrors = errors_;
        if (consumeSignificant("KEYWORD", "else", &elseTok)) {
            if (consumeSignificant("Colon", ":", &colonTok)) {
                ParseTreeNode* blockNode = parseBlock();
                if (blockNode) {
                    addTerminalNode(ptn, elseTok);
                    addTerminalNode(ptn, colonTok);
                    ptn->addChild(blockNode);
                    return ptn;
                }
            }
        }
        currentTokenIndex_ = elseAttemptIndex; errors_ = elseAttemptErrors;
        delete ptn; ptn = new ParseTreeNode("<else_clause>");
    }
    addEpsilonNode(ptn);
    return ptn;
}

ParseTreeNode* Parser::parseWhileStmt() {
    ParseTreeNode* ptn = new ParseTreeNode("<while_stmt>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    Token whileTok, colonTok;
    if (consumeSignificant("KEYWORD", "while", &whileTok)) {
        addTerminalNode(ptn, whileTok);
        ParseTreeNode* exprNode = parseExpr();
        if (exprNode) {
            ptn->addChild(exprNode);
            if (consumeSignificant("Colon", ":", &colonTok)) {
                addTerminalNode(ptn, colonTok);
                ParseTreeNode* blockNode = parseBlock();
                if (blockNode) {
                    ptn->addChild(blockNode);
                    return ptn;
                }
            }
        }
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseForStmt() {
    ParseTreeNode* ptn = new ParseTreeNode("<for_stmt>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    Token forTok, idTok, inTok, colonTok;

    if (consumeSignificant("KEYWORD", "for", &forTok)) {
        addTerminalNode(ptn, forTok);
        if (consumeSignificant("IDENTIFIER", "", &idTok)) {
            addTerminalNode(ptn, idTok, "id");
            if (consumeSignificant("KEYWORD", "in", &inTok)) {
                addTerminalNode(ptn, inTok);
                ParseTreeNode* exprNode = parseExpr();
                if (exprNode) {
                    ptn->addChild(exprNode);
                    if (consumeSignificant("Colon", ":", &colonTok)) {
                        addTerminalNode(ptn, colonTok);
                        ParseTreeNode* blockNode = parseBlock();
                        if (blockNode) {
                            ptn->addChild(blockNode);
                            return ptn;
                        }
                    }
                }
            }
        }
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseFuncDef() {
    ParseTreeNode* ptn = new ParseTreeNode("<func_def>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    Token defTok, idTok, lparenTok, rparenTok, colonTok;

    if (consumeSignificant("KEYWORD", "def", &defTok)) {
        addTerminalNode(ptn, defTok);
        if (consumeSignificant("IDENTIFIER", "", &idTok)) {
            addTerminalNode(ptn, idTok, "id");
            if (consumeSignificant("LParenthese", "(", &lparenTok)) {
                addTerminalNode(ptn, lparenTok);
                ParseTreeNode* paramListNode = parseParamList();
                if(paramListNode) {
                    ptn->addChild(paramListNode);
                    if (consumeSignificant("RParenthese", ")", &rparenTok)) {
                        addTerminalNode(ptn, rparenTok);
                        if (consumeSignificant("Colon", ":", &colonTok)) {
                            addTerminalNode(ptn, colonTok);
                            ParseTreeNode* blockNode = parseBlock();
                            if (blockNode) {
                                ptn->addChild(blockNode);
                                return ptn;
                            }
                        }
                    }
                } else { /* paramListNode creation failed */ }
            }
        }
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseBlock() {
    ParseTreeNode* ptn = new ParseTreeNode("<block>");
    size_t ruleStartIndex = currentTokenIndex_;
    std::vector<std::string> ruleStartErrors = errors_;
    Token indentTok, dedentTok;

    // Consume any NEWLINE tokens
    while (matchExact("NEWLINE")) {
        Token nlTok;
        consumeExact("NEWLINE", "", &nlTok);
    }

    if (consumeExact("INDENT", "", &indentTok)) {
        addTerminalNode(ptn, indentTok);
        ParseTreeNode* stmtListNode = parseStmtList();
        if (stmtListNode) {
            ptn->addChild(stmtListNode);
            if (consumeExact("DEDENT", "", &dedentTok)) {
                addTerminalNode(ptn, dedentTok);
                return ptn;
            } else {
                error("Expected DEDENT at line " + std::to_string(currentToken().line) +
                      ", col " + std::to_string(currentToken().column));
            }
        } else {
            error("Expected statement list in block at line " + std::to_string(currentToken().line) +
                  ", col " + std::to_string(currentToken().column));
        }
    } else {
        error("Expected INDENT at line " + std::to_string(currentToken().line) +
              ", col " + std::to_string(currentToken().column));
    }
    currentTokenIndex_ = ruleStartIndex;
    errors_ = ruleStartErrors;
    delete ptn;
    return nullptr;
}

ParseTreeNode* Parser::parseParamList() {
    ParseTreeNode* ptn = new ParseTreeNode("<param_list>");
    Token idTok;
    if (match("IDENTIFIER")) {
        if (consumeSignificant("IDENTIFIER", "", &idTok)){
            addTerminalNode(ptn, idTok, "id");
            ParseTreeNode* tailNode = parseParamTail();
            if(tailNode) {ptn->addChild(tailNode); return ptn;}
            else { delete ptn; return nullptr; }
        }
        delete ptn; return nullptr;
    } else {
        addEpsilonNode(ptn);
        return ptn;
    }
}
ParseTreeNode* Parser::parseParamTail() {
    ParseTreeNode* ptn = new ParseTreeNode("<param_tail>");
    Token commaTok, idTok;
    if (match("Comma")) {
        if(consumeSignificant("Comma", ",", &commaTok)){
            addTerminalNode(ptn, commaTok);
            if (consumeSignificant("IDENTIFIER", "", &idTok)) {
                addTerminalNode(ptn, idTok, "id");
                ParseTreeNode* recursiveTailNode = parseParamTail();
                if(recursiveTailNode) {ptn->addChild(recursiveTailNode); return ptn;}
                else { delete ptn; return nullptr; }
            } else { delete ptn; return nullptr;}
        }
        delete ptn; return nullptr;
    } else {
        addEpsilonNode(ptn);
        return ptn;
    }
}

ParseTreeNode* Parser::parsePrintStmt() {
    ParseTreeNode* ptn = new ParseTreeNode("<print_stmt>");
    size_t ruleStartIndex = currentTokenIndex_;
    std::vector<std::string> ruleStartErrors = errors_;
    Token printTok, lparenTok, rparenTok;

    std::cout << "parsePrintStmt: Start at index=" << currentTokenIndex_
              << ", token=" << (currentTokenIndex_ < tokens_.size() ? tokens_[currentTokenIndex_].type + ":" + tokens_[currentTokenIndex_].value : "EOF") << std::endl;

    if (consumeSignificant("KEYWORD", "print", &printTok)) {
        std::cout << "parsePrintStmt: Consumed 'print'" << std::endl;
        addTerminalNode(ptn, printTok);
        if (consumeSignificant("LParenthese", "(", &lparenTok)) {
            std::cout << "parsePrintStmt: Consumed '('" << std::endl;
            addTerminalNode(ptn, lparenTok);
            ParseTreeNode* exprNode = parseExpr();
            if (exprNode) {
                std::cout << "parsePrintStmt: Parsed expression" << std::endl;
                ptn->addChild(exprNode);
                if (consumeSignificant("RParenthese", ")", &rparenTok)) {
                    std::cout << "parsePrintStmt: Consumed ')'" << std::endl;
                    addTerminalNode(ptn, rparenTok);
                    return ptn;
                } else {
                    std::cout << "parsePrintStmt: Failed to consume ')'" << std::endl;
                }
            } else {
                std::cout << "parsePrintStmt: Failed to parse expression" << std::endl;
            }
        } else {
            std::cout << "parsePrintStmt: Failed to consume '('" << std::endl;
        }
    } else {
        std::cout << "parsePrintStmt: Failed to consume 'print'" << std::endl;
    }
    std::cout << "parsePrintStmt: Backtracking" << std::endl;
    currentTokenIndex_ = ruleStartIndex;
    errors_ = ruleStartErrors;
    delete ptn;
    return nullptr;
}

ParseTreeNode* Parser::parseReturnStmt() {
    ParseTreeNode* ptn = new ParseTreeNode("<return_stmt>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    Token returnTok;

    if (consumeSignificant("KEYWORD", "return", &returnTok)) {
        addTerminalNode(ptn, returnTok);
        size_t tempIndexForPeek = currentTokenIndex_;
        const Token& nextSigPeek = currentSignificantToken();
        currentTokenIndex_ = tempIndexForPeek;

        if (nextSigPeek.type == "NEWLINE" || nextSigPeek.type == "DEDENT" || nextSigPeek.type == "EOF" || nextSigPeek.type == "Semicolon" ) {
             error("Grammar Error: Expected expression after 'return', found end of statement context (Line: " + std::to_string(nextSigPeek.line) + "). Grammar requires an expression.");
        }
        ParseTreeNode* exprNode = parseExpr();
        if (exprNode) {
            ptn->addChild(exprNode);
            return ptn;
        }
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseExprStmt() {
    ParseTreeNode* ptn = new ParseTreeNode("<expr_stmt>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    ParseTreeNode* exprNode = parseExpr();
    if (exprNode) {
        ptn->addChild(exprNode);
        return ptn;
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseExpr() {
    ParseTreeNode* ptn = new ParseTreeNode("<expr>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    ParseTreeNode* logicExprNode = parseLogicExpr();
    if (logicExprNode) {
        ptn->addChild(logicExprNode);
        return ptn;
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseLogicExpr() {
    ParseTreeNode* ptn = new ParseTreeNode("<logic_expr>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    ParseTreeNode* compExprNode = parseCompExpr();
    if (compExprNode) {
        ptn->addChild(compExprNode);
        ParseTreeNode* tailNode = parseLogicTail();
        if(tailNode) {ptn->addChild(tailNode); return ptn;}
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseLogicTail() {
    ParseTreeNode* ptn = new ParseTreeNode("<logic_tail>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    if (match("KEYWORD", "and") || match("KEYWORD", "or")) {
        ParseTreeNode* logicOpNode = parseLogicOp();
        if (logicOpNode) {
            ptn->addChild(logicOpNode);
            ParseTreeNode* compExprNode = parseCompExpr();
            if (compExprNode) {
                ptn->addChild(compExprNode);
                ParseTreeNode* recursiveTailNode = parseLogicTail();
                if(recursiveTailNode) {ptn->addChild(recursiveTailNode); return ptn;}
            }
        }
        currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
    } else {
        addEpsilonNode(ptn); return ptn;
    }
}

ParseTreeNode* Parser::parseLogicOp() {
    ParseTreeNode* ptn = new ParseTreeNode("<logic_op>");
    Token opTok;
    if (consumeSignificant("KEYWORD", "and", &opTok) || consumeSignificant("KEYWORD", "or", &opTok)) {
        addTerminalNode(ptn, opTok);
        return ptn;
    }
    delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseCompExpr() {
    ParseTreeNode* ptn = new ParseTreeNode("<comp_expr>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    ParseTreeNode* arithExprNode = parseArithExpr();
    if (arithExprNode) {
        ptn->addChild(arithExprNode);
        ParseTreeNode* tailNode = parseCompTail();
        if(tailNode) {ptn->addChild(tailNode); return ptn;}
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseCompTail() {
    ParseTreeNode* ptn = new ParseTreeNode("<comp_tail>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    size_t indexBeforePeek = currentTokenIndex_;
    const Token& peekedToken = currentSignificantToken();
    currentTokenIndex_ = indexBeforePeek;

    if (isCompOperator(peekedToken)) {
        ParseTreeNode* compOpNode = parseCompOp();
        if (compOpNode) {
            ptn->addChild(compOpNode);
            ParseTreeNode* arithExprNode = parseArithExpr();
            if (arithExprNode) {
                ptn->addChild(arithExprNode);
                ParseTreeNode* recursiveTailNode = parseCompTail();
                if(recursiveTailNode) {ptn->addChild(recursiveTailNode); return ptn;}
            }
        }
        currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
    } else {
        addEpsilonNode(ptn); return ptn;
    }
}

ParseTreeNode* Parser::parseCompOp() {
    ParseTreeNode* ptn = new ParseTreeNode("<comp_op>");
    size_t ruleStartIndex = currentTokenIndex_;
    std::vector<std::string> ruleStartErrors = errors_;
    Token opTok;
    const std::string validOps[] = {"==", "!=", "<=", ">=", "<", ">"};

    for (const auto& op : validOps) {
        if (match("OPERATOR", op)) { // Use match to peek without advancing
            if (consumeSignificant("OPERATOR", op, &opTok)) {
                addTerminalNode(ptn, opTok);
                return ptn;
            }
        }
    }

    // Only generate an error if no operator matches
    const Token& curSig = currentSignificantToken();
    currentTokenIndex_ = ruleStartIndex; // Restore index before error
    error("Expected comparison operator (==, !=, <=, >=, <, >), but got '" + curSig.value +
          "' (type: " + curSig.type + ") at line " + std::to_string(curSig.line) +
          ", col " + std::to_string(curSig.column));
    errors_ = ruleStartErrors; // Clear errors from failed attempts
    errors_.push_back(errors_.back()); // Add only the final error
    delete ptn;
    return nullptr;
}

ParseTreeNode* Parser::parseArithExpr() {
    ParseTreeNode* ptn = new ParseTreeNode("<arith_expr>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    ParseTreeNode* termNode = parseTerm();
    if (termNode) {
        ptn->addChild(termNode);
        ParseTreeNode* tailNode = parseAddTail();
        if(tailNode) {ptn->addChild(tailNode); return ptn;}
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseAddTail() {
    ParseTreeNode* ptn = new ParseTreeNode("<add_tail>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    if (match("OPERATOR", "+") || match("OPERATOR", "-")) {
        ParseTreeNode* addOpNode = parseAddOp();
        if (addOpNode) {
            ptn->addChild(addOpNode);
            ParseTreeNode* termNode = parseTerm();
            if (termNode) {
                ptn->addChild(termNode);
                ParseTreeNode* recursiveTailNode = parseAddTail();
                if(recursiveTailNode) {ptn->addChild(recursiveTailNode); return ptn;}
            }
        }
        currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
    } else {
        addEpsilonNode(ptn); return ptn;
    }
}

ParseTreeNode* Parser::parseAddOp() {
    ParseTreeNode* ptn = new ParseTreeNode("<add_op>");
    Token opTok;
    if (consumeSignificant("OPERATOR", "+", &opTok) || consumeSignificant("OPERATOR", "-", &opTok)) {
        addTerminalNode(ptn, opTok);
        return ptn;
    }
    delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseTerm() {
    ParseTreeNode* ptn = new ParseTreeNode("<term>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    ParseTreeNode* factorNode = parseFactor();
    if (factorNode) {
        ptn->addChild(factorNode);
        ParseTreeNode* tailNode = parseMulTail();
        if(tailNode) {ptn->addChild(tailNode); return ptn;}
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseMulTail() {
    ParseTreeNode* ptn = new ParseTreeNode("<mul_tail>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    if (match("OPERATOR", "*") || match("OPERATOR", "/")) {
        ParseTreeNode* mulOpNode = parseMulOp();
        if (mulOpNode) {
            ptn->addChild(mulOpNode);
            ParseTreeNode* factorNode = parseFactor();
            if (factorNode) {
                ptn->addChild(factorNode);
                ParseTreeNode* recursiveTailNode = parseMulTail();
                if(recursiveTailNode) {ptn->addChild(recursiveTailNode); return ptn;}
            }
        }
         currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
    } else {
        addEpsilonNode(ptn); return ptn;
    }
}

ParseTreeNode* Parser::parseMulOp() {
    ParseTreeNode* ptn = new ParseTreeNode("<mul_op>");
    Token opTok;
    if (consumeSignificant("OPERATOR", "*", &opTok) || consumeSignificant("OPERATOR", "/", &opTok)) {
        addTerminalNode(ptn, opTok);
        return ptn;
    }
    delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseFactor() {
    ParseTreeNode* ptn = new ParseTreeNode("<factor>");
    size_t factorStartIndex = currentTokenIndex_;
    std::vector<std::string> factorStartErrors = errors_;
    Token tok;

    if (match("LParenthese")) {
        size_t parenExprAttemptIndex = currentTokenIndex_;
        Token lparenTokVal;
        if (consumeSignificant("LParenthese", "(", &lparenTokVal)) {
            ParseTreeNode* exprNode = parseExpr();
            if (exprNode) {
                Token rparenTokVal;
                if (consumeSignificant("RParenthese", ")", &rparenTokVal)) {
                    addTerminalNode(ptn, lparenTokVal);
                    ptn->addChild(exprNode);
                    addTerminalNode(ptn, rparenTokVal);
                    return ptn;
                }
            }
        }
        currentTokenIndex_ = parenExprAttemptIndex;
        errors_ = factorStartErrors; // Could be more specific to parenExprStartErrors if defined
    }
    else if (match("NUMBER")) {
        if(consumeSignificant("NUMBER", "", &tok)) { addTerminalNode(ptn, tok, "num"); return ptn; }
    }
    else if (match("STRING")) {
         if(consumeSignificant("STRING", "", &tok)) { addTerminalNode(ptn, tok, "string"); return ptn; }
    }
    else if (match("IDENTIFIER")) {
        size_t idPathStartIndex = currentTokenIndex_;
        currentSignificantToken(); // Advances index to the ID
        size_t idTokenActualIndex = currentTokenIndex_ -1; // Index of the ID token
        if (idTokenActualIndex < idPathStartIndex) idTokenActualIndex = idPathStartIndex; // ensure it's not before start
        if (idTokenActualIndex >= tokens_.size() && !tokens_.empty()) idTokenActualIndex = tokens_.size()-1; // bounds

        const Token& nextSig = lookaheadSignificant(1);

        if (nextSig.type == "LParenthese") {
            currentTokenIndex_ = idTokenActualIndex;
            ParseTreeNode* funcCallNode = parseFuncCall();
            if (funcCallNode) { ptn->addChild(funcCallNode); return ptn; }
            currentTokenIndex_ = factorStartIndex; errors_ = factorStartErrors; delete ptn; return nullptr;
        } else {
            currentTokenIndex_ = idTokenActualIndex;
            if(consumeSignificant("IDENTIFIER", "", &tok)){ addTerminalNode(ptn, tok, "id"); return ptn; }
        }
    }
    else if (match("KEYWORD", "True") || match("KEYWORD", "False") || match("KEYWORD", "None")) {
        size_t keywordStartIndex = currentTokenIndex_;
        std::string val = currentSignificantToken().value;
        currentTokenIndex_ = keywordStartIndex;
        if(consumeSignificant("KEYWORD", val, &tok)) { addTerminalNode(ptn, tok); return ptn;}
    }

    delete ptn;
    currentTokenIndex_ = factorStartIndex;
    errors_ = factorStartErrors;
    const Token& curSigFactorStart = currentToken(factorStartIndex);
    bool errorAddedByAlternatives = errors_.size() > factorStartErrors.size();

    if(!errorAddedByAlternatives && !isAtEnd() &&
        curSigFactorStart.type != "RParenthese" && curSigFactorStart.type != "Comma" && curSigFactorStart.type != "Colon" &&
        curSigFactorStart.type != "DEDENT" && curSigFactorStart.type != "NEWLINE" ) {
        error("Syntax Error: Unexpected token '" + curSigFactorStart.value + "' (type " + curSigFactorStart.type + ") when expecting a factor at line " + std::to_string(curSigFactorStart.line) + " col " + std::to_string(curSigFactorStart.column));
    }
    return nullptr;
}

ParseTreeNode* Parser::parseFuncCall() {
    ParseTreeNode* ptn = new ParseTreeNode("<func_call>");
    size_t ruleStartIndex = currentTokenIndex_; std::vector<std::string> ruleStartErrors = errors_;
    Token idTok, lparenTok, rparenTok;

    if (consumeSignificant("IDENTIFIER", "", &idTok)) {
        if (consumeSignificant("LParenthese", "(", &lparenTok)) {
            ParseTreeNode* argListNode = parseArgList();
            if (argListNode) {
                // ptn->addChild(argListNode); // Add later in correct order
                if (consumeSignificant("RParenthese", ")", &rparenTok)) {
                    addTerminalNode(ptn, idTok, "id");
                    addTerminalNode(ptn, lparenTok);
                    ptn->addChild(argListNode); // argListNode is a single node (possibly with children or epsilon)
                    addTerminalNode(ptn, rparenTok);
                    return ptn;
                } else { if (argListNode) delete argListNode; }
            }
        }
    }
    currentTokenIndex_ = ruleStartIndex; errors_ = ruleStartErrors; delete ptn; return nullptr;
}

ParseTreeNode* Parser::parseArgList() {
    ParseTreeNode* ptn = new ParseTreeNode("<arg_list>");
    size_t ruleStartIndex = currentTokenIndex_;
    std::vector<std::string> ruleStartErrors = errors_;

    // Empty argument list
    if (match("RParenthese")) {
        addEpsilonNode(ptn);
        return ptn;
    }

    // Parse first argument
    ParseTreeNode* exprNode = parseExpr();
    if (!exprNode) {
        error("Expected expression in argument list at line " +
              std::to_string(currentToken().line) + ", col " +
              std::to_string(currentToken().column));
        currentTokenIndex_ = ruleStartIndex;
        errors_ = ruleStartErrors;
        delete ptn;
        return nullptr;
    }

    ptn->addChild(exprNode);
    ParseTreeNode* tailNode = parseArgTail();
    if (tailNode) {
        ptn->addChild(tailNode);
        return ptn;
    }

    error("Invalid argument list tail at line " +
          std::to_string(currentToken().line) + ", col " +
          std::to_string(currentToken().column));
    currentTokenIndex_ = ruleStartIndex;
    errors_ = ruleStartErrors;
    delete ptn;
    return nullptr;
}

ParseTreeNode* Parser::parseArgTail() {
    ParseTreeNode* ptn = new ParseTreeNode("<arg_tail>");
    size_t ruleStartIndex = currentTokenIndex_;
    std::vector<std::string> ruleStartErrors = errors_;
    Token commaTok;

    if (match("Comma")) {
        if (consumeSignificant("Comma", ",", &commaTok)) {
            addTerminalNode(ptn, commaTok);
            ParseTreeNode* exprNode = parseExpr();
            if (exprNode) {
                ptn->addChild(exprNode);
                ParseTreeNode* recursiveTailNode = parseArgTail();
                if (recursiveTailNode) {
                    ptn->addChild(recursiveTailNode);
                    return ptn;
                }
                error("Invalid argument list continuation at line " +
                      std::to_string(currentToken().line) + ", col " +
                      std::to_string(currentToken().column));
            } else {
                error("Expected expression after comma in argument list at line " +
                      std::to_string(currentToken().line) + ", col " +
                      std::to_string(currentToken().column));
            }
        }
        currentTokenIndex_ = ruleStartIndex;
        errors_ = ruleStartErrors;
        delete ptn;
        return nullptr;
    }

    addEpsilonNode(ptn);
    return ptn;
}




// --- GUI Code ---
// (Global HWNDs, InsertTreeNode, WndProc, WinMain - as provided in the previous GUI response)
// Global HWNDs (defined once)
HWND hInput, hOutput, hSymbol, hParseTree, hSyntaxErrors, hErrors;

// Insert into TreeView
void InsertTreeNode(HWND hTree, HTREEITEM parent, ParseTreeNode* node) {
    if (!node) return;
    TVINSERTSTRUCTA tvis = {};
    tvis.hParent = parent;
    tvis.hInsertAfter = TVI_LAST;
    tvis.item.mask = TVIF_TEXT;
    tvis.item.pszText = const_cast<LPSTR>(node->name.c_str());
    HTREEITEM item = (HTREEITEM)SendMessageA(hTree, TVM_INSERTITEMA, 0, (LPARAM)&tvis);
    for (auto* ch : node->children) {
        if (ch) {
            InsertTreeNode(hTree, item, ch);
        }
    }
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    switch (msg) {
    case WM_CREATE:
        hInput = CreateWindowA("EDIT", "", WS_CHILD | WS_VISIBLE | WS_BORDER | WS_VSCROLL | WS_HSCROLL |
            ES_MULTILINE | ES_AUTOVSCROLL | ES_AUTOHSCROLL, 10, 10, 400, 150,
            hwnd, (HMENU)IDC_INPUT, NULL, NULL);
        CreateWindowA("BUTTON", "Lexical Analysis", WS_CHILD | WS_VISIBLE |
            BS_PUSHBUTTON, 420, 10, 150, 30, hwnd, (HMENU)IDC_LEX_BUTTON, NULL, NULL);
        CreateWindowA("BUTTON", "Run Parser", WS_CHILD | WS_VISIBLE |
            BS_PUSHBUTTON, 420, 50, 150, 30, hwnd, (HMENU)IDC_PARSE_BUTTON, NULL, NULL);
        hOutput = CreateWindowA("EDIT", "", WS_CHILD | WS_VISIBLE | WS_BORDER | WS_VSCROLL | WS_HSCROLL |
            ES_MULTILINE | ES_AUTOVSCROLL | ES_READONLY, 10, 170, 400, 150,
            hwnd, (HMENU)IDC_OUTPUT, NULL, NULL);
        hSymbol = CreateWindowA("EDIT", "", WS_CHILD | WS_VISIBLE | WS_BORDER | WS_VSCROLL | WS_HSCROLL |
            ES_MULTILINE | ES_AUTOVSCROLL | ES_READONLY, 420, 90, 150, 150,
            hwnd, (HMENU)IDC_SYMBOL, NULL, NULL);
        hErrors = CreateWindowA("EDIT", "", WS_CHILD | WS_VISIBLE | WS_BORDER | WS_VSCROLL | WS_HSCROLL |
            ES_MULTILINE | ES_AUTOVSCROLL | ES_READONLY, 10, 330, 560, 100,
            hwnd, (HMENU)IDC_ERRORS, NULL, NULL);
        hParseTree = CreateWindowExA(0, WC_TREEVIEWA, NULL, WS_CHILD | WS_VISIBLE |
            WS_BORDER | TVS_HASLINES | TVS_HASBUTTONS | TVS_LINESATROOT,
            10, 440, 400, 250, hwnd, (HMENU)IDC_PARSE_TREE, GetModuleHandle(NULL), NULL);
        hSyntaxErrors = CreateWindowA("EDIT", "", WS_CHILD | WS_VISIBLE | WS_BORDER | WS_VSCROLL | WS_HSCROLL |
            ES_MULTILINE | ES_AUTOVSCROLL | ES_READONLY, 420, 250, 150, 440,
            hwnd, (HMENU)IDC_SYNTAX_ERRORS, NULL, NULL);
        break;

    case WM_COMMAND:
        if (LOWORD(wParam) == IDC_LEX_BUTTON || LOWORD(wParam) == IDC_PARSE_BUTTON) {
            int len = GetWindowTextLengthA(hInput);
            if (len > 0) {
                std::vector<char> buf(len + 1);
                GetWindowTextA(hInput, buf.data(), len + 1);
                std::string src(buf.data());

                std::vector<std::string> lexErrForDisplay;
                std::vector<Symbol> symTab;
                symTab.clear();
                lexErrForDisplay.clear();

                // Assuming your lexer functions are correctly defined and linked
                std::vector<Token> toks = tokenize(src, lexErrForDisplay, symTab);
                buildSymbolTableFromTokens(toks, symTab);
                // Using your original displayResults signature (no HWNDs passed directly)
                // because you pasted that version of displayResults in the last code block.
                displayResults(toks, symTab, lexErrForDisplay);

                SetWindowTextA(hSyntaxErrors, "");
                SendMessageA(hParseTree, TVM_DELETEITEM, 0, (LPARAM)TVI_ROOT);

                if (LOWORD(wParam) == IDC_PARSE_BUTTON) {
                    std::vector<std::string> syntaxErrForParser;
                    Parser parser(toks, syntaxErrForParser);

                    ParseTreeNode* cstRoot = parser.parseProgram();

                    if (cstRoot) {
                        InsertTreeNode(hParseTree, TVI_ROOT, cstRoot);
                        delete cstRoot;
                    }

                    std::ostringstream se;
                    se << "Syntax Errors:\r\n";
                    if (parser.getErrors().empty() && cstRoot) { // Check if CST root is valid too for "no errors"
                         se << "(No syntax errors detected)\r\n";
                    } else if (parser.getErrors().empty() && !cstRoot) {
                         se << "(Parsing failed without specific errors, check CST if partial)\r\n";
                    }
                    else {
                        for (const auto& e : parser.getErrors()) {
                            se << e << "\r\n";
                        }
                    }
                    SetWindowTextA(hSyntaxErrors, se.str().c_str());
                }
            } else { // Input text box is empty
                displayResults({}, {}, {});
                SetWindowTextA(hSyntaxErrors, "");
                SendMessageA(hParseTree, TVM_DELETEITEM, 0, (LPARAM)TVI_ROOT);
            }
        }
        break;

    case WM_DESTROY:
        PostQuitMessage(0);
        break;
    default:
        return DefWindowProc(hwnd, msg, wParam, lParam);
    }
    return 0;
}

int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nShowCmd)
{
    INITCOMMONCONTROLSEX icex;
    icex.dwSize = sizeof(INITCOMMONCONTROLSEX);
    icex.dwICC = ICC_TREEVIEW_CLASSES;
    InitCommonControlsEx(&icex);

    WNDCLASSA wc = {};
    wc.lpfnWndProc = WndProc;
    wc.hInstance = hInst;
    wc.lpszClassName = "PythonCompilerWindow";
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
    wc.style = CS_HREDRAW | CS_VREDRAW;

    if (!RegisterClassA(&wc)) {
        MessageBoxA(NULL, "Window Registration Failed!", "Error", MB_ICONEXCLAMATION | MB_OK);
        return 0;
    }

    HWND hwnd = CreateWindowExA(
        0, "PythonCompilerWindow", "Python Compiler (Lexer+Parser+CST)",
        WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT, CW_USEDEFAULT, 800, 750,
        NULL, NULL, hInst, NULL
    );

    if (!hwnd) {
        MessageBoxA(NULL, "Window Creation Failed!", "Error", MB_ICONEXCLAMATION | MB_OK);
        return 0;
    }

    ShowWindow(hwnd, nShowCmd);
    UpdateWindow(hwnd);

    MSG msg = {};
    while (GetMessage(&msg, NULL, 0, 0) > 0) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return (int)msg.wParam;
}
