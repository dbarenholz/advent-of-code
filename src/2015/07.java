import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;


class AOC {
  BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
  BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));
  Map<String, Integer> bindings = new HashMap<>();
  Map<String, Expression> expressions = new HashMap<>();


  private void part1(List<String> lines) throws Exception {
    for (String line : lines) {
      String[] tokens = line.split("->");

      String varName  = tokens[1].strip();
      String exprStr  = tokens[0].strip();

      Expression expr = parse(exprStr);
      expressions.put(varName, expr);

      if (expr.type == ExpressionType.LITERAL) {
        bindings.put(varName, expr.literal.value);
      }
    }

    int ans = expressions.get("a").evaluate();
    bindings.put("a", ans);

    w.write("Part 1: " + ans + "\n");
    w.flush();
  }

  private void part2(List<String> lines) throws Exception {
    // Do everything from part 1; done in run()!

    int val = bindings.get("a");
    // reset all others to nothing
    bindings.clear();
    // Override wire 'b' to become wire 'a'
    bindings.put("b", val);

    // Then find the value for 'a' again
    int ans = expressions.get("a").evaluate();

    w.write("Part 2: " + ans + "\n");
    w.flush();
  }

  private void run() throws Exception {
    List<String> lines = new ArrayList<>();
    String line;
    while ((line = r.readLine()) != null) {
      if (line.equals("")) {
        break;
      }
      lines.add(line);
    }

    part1(lines);
    part2(lines);
  }

  public static void main(String[] a) throws Exception {
    (new AOC()).run();
  }

  final Set<Character> alphabet = Set.of('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z');

  private boolean isNumber(String str) {
    for (char a: alphabet) {
      if (str.contains(a + "")) {
        return false;
      }
    }

    return true;
  }

  static int clamp(int x) {
    final int MAX_VALUE = 65535;
    return x < 0 ? MAX_VALUE + x + 1 : x;
  }

  private Expression parse(String expr) throws Exception {
    String[] tokens = expr.split(" ");

    // LITERALS
    if (tokens.length == 1) {
      // If it is a number, set it to the number
      if (isNumber(expr)) {
        return new Expression( new Literal(Integer.parseInt(expr)) );
      }
      // Otherwise, we have a variable name
      return new Expression( new Variable(expr) );
    }


    // unary operations
    if (tokens.length == 2) {
      String operation = tokens[0];
      String variable  = tokens[1];
      switch (operation) {
        case "NOT":
          return new Expression(new UnaryOperator(UnaryOperatorType.NOT, parse(variable)));
      }
    }

    // binary operations
    if (tokens.length == 3) {
      String lhs       = tokens[0];
      String operation = tokens[1];
      String rhs       = tokens[2];

      switch(operation) {
        case "AND":
          return new Expression(new BinaryOperator(BinaryOperatorType.AND,    parse(lhs), parse(rhs)));
        case "OR":
          return new Expression(new BinaryOperator(BinaryOperatorType.OR,     parse(lhs), parse(rhs)));
        case "LSHIFT":
          return new Expression(new BinaryOperator(BinaryOperatorType.LSHIFT, parse(lhs), parse(rhs)));
        case "RSHIFT":
          return new Expression(new BinaryOperator(BinaryOperatorType.RSHIFT, parse(lhs), parse(rhs)));
      }
    }

    throw new Exception("Cannot parse expr:'" + expr + "'.");
  }

  enum UnaryOperatorType   { NOT }
  enum BinaryOperatorType  { AND, OR, LSHIFT, RSHIFT }
  enum ExpressionType      { LITERAL, BINARY_OP , UNARY_OP, VARIABLE }

  class Variable {
    String name;

    public Variable(String name) {
      this.name = name;
    }

    int evaluate() throws Exception {
      if (! bindings.containsKey(this.name)) {
        bindings.put(this.name, expressions.get(this.name).evaluate());
      }
      // System.out.println("Variable: '" + this.name + "' = " + bindings.get(this.name));
      return clamp(bindings.get(this.name));
    }

    @Override
    public String toString() {
      return this.name;
    }
  }

  class Literal {
    int value;

    public Literal(int value) {
      this.value = value;
    }


    int evaluate() {
      // System.out.println("Literal: " + this.value);
      return (short) this.value;
    }

    @Override
    public String toString() {
      return this.value + "";
    }
  }

  class BinaryOperator {
    BinaryOperatorType op;
    Expression lhs;
    Expression rhs;

    public BinaryOperator(BinaryOperatorType type, Expression lhs, Expression rhs) {
      this.op = type;
      this.lhs = lhs;
      this.rhs = rhs;
    }

    int evaluate() throws Exception {
      // NOTE: Java and unsigned numbers is a pain, so I do things manually
      int lhs = clamp(this.lhs.evaluate());
      int rhs = clamp(this.rhs.evaluate());

      int res = -1;
      switch(op) {
        case OR:
          res = clamp(lhs | rhs);
          // System.out.println(op + " (" + this.lhs.toString() + "=" + lhs + ") (" + this.rhs.toString() + "=" + rhs + ") -> " + res);
          return res;
        case AND:
          res = clamp(lhs & rhs);
          // System.out.println(op + " (" + this.lhs.toString() + "=" + lhs + ") (" + this.rhs.toString() + "=" + rhs + ") -> " + res);
          return res;
        case LSHIFT:
          res = clamp(lhs << rhs);
          // System.out.println(op + " (" + this.lhs.toString() + "=" + lhs + ") (" + this.rhs.toString() + "=" + rhs + ") -> " + res);
          return res;
        case RSHIFT:
          res = clamp(lhs >> rhs);
          // System.out.println(op + " (" + this.lhs.toString() + "=" + lhs + ") (" + this.rhs.toString() + "=" + rhs + ") -> " + res);
          return res;
      }

      throw new UnsupportedOperationException("when evaluating a binary operation");
    }

    @Override
    public String toString() {
      switch(op) {
        case OR:
          return "" + op + " (" + this.lhs.toString() + ") (" + this.rhs.toString() + ")";
        case AND:
          return "" + op + " (" + this.lhs.toString() + ") (" + this.rhs.toString() + ")";
        case LSHIFT:
          return "" + op + " (" + this.lhs.toString() + ") (" + this.rhs.toString() + ")";
        case RSHIFT:
          return "" + op + " (" + this.lhs.toString() + ") (" + this.rhs.toString() + ")";
      }

      return "";
    }
  }

  class UnaryOperator {
    UnaryOperatorType type;
    Expression expr;

    public UnaryOperator(UnaryOperatorType type, Expression expr) {
      this.type = type;
      this.expr = expr;
    }

    int evaluate() throws Exception {
      switch(type) {
        case NOT:
          int res = clamp(~ this.expr.evaluate());
          // System.out.println("NOT (" + this.expr.toString() + "=" + this.expr.evaluate() + ") -> " + res);
          return res;
      }

      throw new UnsupportedOperationException("when evaluating a unary operation");
    }

    @Override
    public String toString() {
      switch(type) {
        case NOT:
          return "" + type + "(" + expr + ")";
      }

      return "";
    }
  }

  class Expression {
    ExpressionType type;
    Literal literal;
    BinaryOperator binaryOperator;
    UnaryOperator unaryOperator;
    Variable variable;

    public Expression(Variable var) {
      this.type = ExpressionType.VARIABLE;
      this.variable = var;
    }

    public Expression(Literal lit) {
      this.type = ExpressionType.LITERAL;
      this.literal = lit;
    }

    public Expression(UnaryOperator unaryOperator) {
      this.type = ExpressionType.UNARY_OP;
      this.unaryOperator = unaryOperator;
    }

    public Expression(BinaryOperator binaryOperator) {
      this.type = ExpressionType.BINARY_OP;
      this.binaryOperator = binaryOperator;
    }

    int evaluate() throws Exception {
      switch (type) {
        case LITERAL:
          // System.out.println("Expression.Literal");
          return this.literal.evaluate();
        case UNARY_OP:
          // System.out.println("Expression.Unary");
          return this.unaryOperator.evaluate();
        case BINARY_OP:
          // System.out.println("Expression.Binary");
          return this.binaryOperator.evaluate();
        case VARIABLE:
          // System.out.println("Expression.Variable");
          return this.variable.evaluate();
      }

      throw new UnsupportedOperationException("when evaluating an expression");
    }

    @Override
    public String toString() {
      switch (type) {
        case LITERAL:
          return "" + type + "(" + this.literal.toString() + ")";
        case UNARY_OP:
          return "" + type + "(" + this.unaryOperator.toString() + ")";
        case BINARY_OP:
          return "" + type + "(" + this.binaryOperator.toString() + ")";
        case VARIABLE:
          return "" + type + "(" + this.variable.toString() + ")";
      }

      return "";
    }
  }
}
