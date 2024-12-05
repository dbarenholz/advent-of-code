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
  int[][] grid = emptyGrid(1000, 1000);

  private int[][] emptyGrid(int n, int m) {
    int[][] grid = new int[n][m];

    for (int i = 0 ; i < n; i++) {
      for (int j = 0; j < m; j++) {
        grid[i][j] = 0;
      }
    }

    return grid;
  }

  private Action actionOf(String token, boolean part2) {
    if (token.equals("on")) {
      return part2 ? Action.INC : Action.ON;
    }
    if (token.equals("off")) {
      return part2 ? Action.DEC : Action.OFF;
    }

    return part2 ? Action.INCINC : Action.TOGGLE;
  }

  private void doActionOnCell(Action action, int i, int j) {
    if (action != null) {
      switch(action) {
        case ON:
          grid[i][j] = 1;
          break;
        case OFF:
          grid[i][j] = 0;
          break;
        case TOGGLE:
          doActionOnCell(grid[i][j] == 0 ? Action.ON : Action.OFF, i, j);
          break;
        case INC:
          grid[i][j] +=1;
          break;
        case DEC:
          grid[i][j] = grid[i][j] - 1 < 0 ? 0 : grid[i][j] - 1;
          break;
        case INCINC:
          grid[i][j] +=2;
          break;
      }
    }
  }

  private void doAction(Action action, String[] from, String[] to) {
    int fromI = Integer.parseInt(from[0]);
    int fromJ = Integer.parseInt(from[1]);
    int toI   = Integer.parseInt(to[0]);
    int toJ   = Integer.parseInt(to[1]);

    for (int i = fromI; i <= toI; i++) {
      for (int j = fromJ; j <= toJ; j++) {
        doActionOnCell(action, i, j);
      }
    }
  }

  private void part1(List<String> lines) throws IOException {
    for (String line : lines) {
      String[] tokens = line.split(" ");
      Action action = actionOf(tokens[1], false);
      String[] from = action == Action.TOGGLE
        ? tokens[1].split(",")
        : tokens[2].split(",");
      String[] to = action == Action.TOGGLE
        ? tokens[3].split(",")
        : tokens[4].split(",");

      doAction(action, from, to);
    }

    long ans = 0L;

    for (int i = 0 ; i < 1000; i++) {
      for (int j = 0; j < 1000; j++) {
        ans += grid[i][j];
      }
    }

    w.write("Part 1: " + ans + "\n");
    w.flush();
  }

  private void part2(List<String> lines) throws IOException {
    for (String line : lines) {
      String[] tokens = line.split(" ");
      Action action = actionOf(tokens[1], true);
      String[] from = action == Action.INCINC
        ? tokens[1].split(",")
        : tokens[2].split(",");
      String[] to = action == Action.INCINC
        ? tokens[3].split(",")
        : tokens[4].split(",");

      doAction(action, from, to);
    }

    long ans = 0L;

    for (int i = 0 ; i < 1000; i++) {
      for (int j = 0; j < 1000; j++) {
        ans += grid[i][j];
      }
    }

    w.write("Part 2: " + ans + "\n");
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
    part2(lines);
  }

  public static void main(String[] a) throws IOException {
    (new AOC()).run();
  }

}

enum Action {
  OFF,
  ON,
  TOGGLE,
  INC,
  DEC,
  INCINC
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
