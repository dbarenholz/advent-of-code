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

  private void part1(List<String> lines) throws IOException {
    long ans = 0L;

    w.write("Part 1: " + ans + "\n");
    w.flush();
  }

  private void part2(List<String> lines) throws IOException {
    long ans = 0L;

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
