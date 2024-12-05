import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

class template {
  BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
  BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));

  private boolean match(String a) {
    return a.equals("XMAS") || a.equals("SAMX");
  }

  private long getMatchesFor(String line) {
    long ans = 0L;
    for (int i = 0; i < line.length() - 3; i++) {
      String substr = line.substring(i, i+4);
      if (match(substr)) {
        ans += 1;
      }
    }
    return ans;
  }

  private long horizontal(String[] lines) throws IOException {
    long ans = 0L;
    for (String line : lines) {
      ans += getMatchesFor(line);
    }
    return ans;
  }

  private char[][] toCharArr(String[] lines) {
    char[][] arr = new char[lines.length][lines[0].length()];
    for (int i = 0; i < lines.length; i++) {
      arr[i] = lines[i].toCharArray();
    }
    return arr;
  }

  private String[] asStringArr(char[][] arr) {
    String[] lines = new String[arr.length];
    for (int i = 0; i < arr.length; i++) {
      lines[i] = arr[i].toString();
    }
    return lines;
  }

  private String[] transposed(String[] lines) throws IOException {
    String[] transposed = new String[lines[0].length()];
    Arrays.fill(transposed, "");

    for (String line : lines) {
      for (int i = 0; i < line.length(); i++) {
        transposed[i] += line.charAt(i);
      }
    }

    return transposed;
  }

  private long vertical(String[] lines) throws IOException {
    return horizontal(transposed(lines));
  }

  private String getDiagonal(String[] lines, int ROWS, int COLS, int r, int c) throws IOException {
    String diag = "";
    while (r < ROWS  && c < COLS ) {
      diag += lines[r++].charAt(c++);
    }
    return diag;
  }

  private String reverse(String str) {
    return new StringBuilder(str).reverse().toString();
  }

  private String[] reverse(String[] arr) {
    String [] res = new String[arr.length];
    for (int i =0; i < arr.length; i ++) {
      res[i] = reverse(arr[i]);
    }
    return res;
  }

  private List<String> getDiagonals(String[] lines) throws IOException {
    List<String> diags = new ArrayList<>();

    int ROWS = lines.length;
    int COLS = lines[0].length();

    for (int row = ROWS - 1; row >= 0; --row) {
      String diagonal = getDiagonal(lines, ROWS, COLS, row, 0);
      diags.add(diagonal);
    }
    for (int col = 1; col < COLS; ++col) {
      String diagonal = getDiagonal(lines, ROWS, COLS, 0, col);
      diags.add(diagonal);
    }

    return diags;
  }

  long diagonal(String[] lines) throws IOException {
    long ans = 0L;

    List<String> diags = getDiagonals(lines);
    List<String> antiDiags = getDiagonals(reverse(lines));

    List<String> checkMe = new ArrayList<>();
    checkMe.addAll(diags);
    checkMe.addAll(antiDiags);

    for (String d : checkMe) {
      if (d.length() >= 4) {
        ans += getMatchesFor(d);
      }
    }

    return ans;
  }

  private void part1(String[] lines) throws IOException {
    long ans = horizontal(lines) + vertical(lines) + diagonal(lines);

    w.write("Part 1: " + ans + "\n");
    w.flush();
  }

  private boolean validXMas(char topLeft, char topRight, char botLeft, char botRight) {
    String diagonalOne = "" + topLeft + botRight;
    String diagonalTwo = "" + topRight + botLeft;

    return (diagonalOne.equals("MS") || diagonalOne.equals("SM"))
      && (diagonalTwo.equals("MS") || diagonalTwo.equals("SM"));
  }

  private long xmas(String[] lines) {
    long ans = 0L;
    char[][] arr = toCharArr(lines);

    // a valid 'A' can never be on an edge
    for (int i = 1; i < arr.length - 1; i++) {
      for (int j = 1; j < arr[i].length - 1; j++) {
        if (arr[i][j] == 'A') {
          if (validXMas(
                arr[i-1][j-1], // top left
                arr[i-1][j+1], // top right
                arr[i+1][j-1], // bot left
                arr[i+1][j+1]  // bot right
          )) {
            ans += 1;
          }
        }
      }
    }

    return ans;
  }

  private void part2(String[] lines) throws IOException {
    long ans = xmas(lines);

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
    String[] arr = lines.toArray(new String[0]);

    part1(arr);
    part2(arr);
  }

  public static void main(String[] a) throws Exception {
    (new template()).run();
  }
}

