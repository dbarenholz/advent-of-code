import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

class AOC {
  BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
  BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));

  private char[][] fromLines(List<String> lines) {
    int rows = lines.size();
    int cols = lines.get(0).length();

    char[][] grid = new char[rows][cols];

    for (int r = 0; r < rows; r ++) {
      for (int c = 0; c < cols; c ++) {
        grid[r][c] = lines.get(r).charAt(c);
      }
    }

    return grid;
  }

  private boolean isGuard(char v) {
    return v == '^' || v == 'v' || v == '<' || v == '>';
  }

  private Character turn(char guard) {
    switch(guard) {
      case '^': return '>';
      case '>': return 'v';
      case 'v': return '<';
      case '<': return '^';
      default : return null;
    }
  }

  private char[][] step(char[][] old, Index guard) {
    char[][] result = old.clone();
    char direction  = result[guard.row][guard.col];

    if (!hasNeighbour(result, guard, direction)) {
      result[guard.row][guard.col] = 'X';
      return result;
    }

    Pair<Index, Character> neighbour = getNeighbour(result, guard, direction);
    Index idx = neighbour.getA();
    char elem = neighbour.getB();

    if (elem == '#') {
      result[guard.row][guard.col] = (char) turn(direction);
      return result;
    }

    result[guard.row][guard.col] = 'X';
    result[idx.row][idx.col] = direction;

    return result;
  }

  private Index getGuard(char[][] grid) {
    for (int r = 0; r < grid.length; r++) {
      for (int c = 0; c < grid[0].length; c++) {
        if (isGuard(grid[r][c])) {
          return new Index(r, c);
        }
      }
    }

    return null;
  }

  private boolean hasNeighbour(char[][] grid, Index pos, char direction) {
    int r = pos.row;
    int c = pos.col;

    switch(direction) {
      case '^':
        r = r - 1;
        break;
      case '>':
        c = c + 1;
        break;
      case 'v':
        r = r + 1;
        break;
      case '<':
        c = c - 1;
        break;
    }

    return r < grid.length && c < grid[0].length;
  }

  private Pair<Index, Character> getNeighbour(char[][] grid, Index pos, char direction) {
    int r = pos.row;
    int c = pos.col;

    switch(direction) {
      case '^': return new Pair<>(new Index(r-1,   c), grid[r-1][  c]);
      case '>': return new Pair<>(new Index(r,   c+1), grid[r  ][c+1]);
      case 'v': return new Pair<>(new Index(r+1,   c), grid[r+1][  c]);
      case '<': return new Pair<>(new Index(r,   c-1), grid[r  ][c-1]);
      default : return null;
    }
  }

  private void part1(List<String> lines) throws IOException {
    char[][] grid = fromLines(lines);
    Index guard = getGuard(grid);

    while (guard != null) {
      grid = step(grid, guard);
      guard = getGuard(grid);
    }

    long ans = 0L;
    for (char[] arr : grid) {
      for (char c : arr) {
        if (c == 'X') {
          ans += 1;
        }
      }
    }

    w.write("Part 1: " + ans + "\n");
    w.flush();
  }


  private void run() throws IOException {
    List<String> lines = new ArrayList<>();
    String line;
    while ((line = r.readLine()) != null) {
      if (line.equals("")) {
        break;
      }
      lines.add(line);
    }

    part1(lines);
  }

  public static void main(String[] a) throws IOException {
    (new AOC()).run();
  }

}

class Index {
  int row;
  int col;

  public Index(int i, int j) {
    this.row = i;
    this.col = j;
  }

}

class Pair<A, B> {
  A a;
  B b;

  public Pair(A a, B b) {
    this.a = a;
    this.b = b;
  }

  public A getA() { return a; }
  public B getB() { return b; }
  public void setA(A a) { this.a = a; }
  public void setB(B b) { this.b = b; }
}
