import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

class template {
  BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
  BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));

  private List<String> findMatches(String regex, String content) {
    return findMatches(regex, 0, content);
  }
  private List<String> findMatches(String regex, int flags,  String content) {
    Matcher m = Pattern.compile(regex, flags).matcher(content);
    List<String> l = new ArrayList<>();
    while (m.find()) {
      l.add(m.group());
    }
    return l;
  }

  private List<Pair<Integer, String>> findMatchesWithIndex(String regex, String content) throws IOException {
    return findMatchesWithIndex(regex, 0, content);
  }
  private List<Pair<Integer, String>> findMatchesWithIndex(String regex,int flags, String content) throws IOException {
    Matcher m = Pattern.compile(regex, flags).matcher(content);
    List<Pair<Integer, String>> l = new ArrayList<>();
    while (m.find()) {
      l.add(new Pair<Integer, String>(m.start(), m.group()));
    }
    return l;
  }

  private void part1(String memory) throws IOException {
    long ans = findMatches("mul\\((\\d*),(\\d*)\\)", memory).stream().mapToLong(instruction -> {
      // NOTE: I could also use the groups properly, but I'm too lazy to figure out how to regex in java
      int commaIndex = instruction.indexOf(',');
      long a = Long.parseLong(instruction.substring(4, commaIndex));
      long b = Long.parseLong(instruction.substring(commaIndex + 1, instruction.length() - 1));
      return a * b;
    }).sum();

    w.write("Part 1: " + ans + "\n");
    w.flush();
  }

  List<Pair<Integer, Integer>> enabledRange(int n, List<Integer> donts, List<Integer> dos) throws IOException {
    List<Pair<Integer, Integer>> l = new ArrayList<>();

    boolean prevIsDo = true;
    int x = 0;
    for (int i = 0; i < n; i ++) {
      // If it is a dont, and the previous instruction is do, then close the range
      if (donts.contains(i) && prevIsDo) {
        l.add(new Pair<>(x, i));
        prevIsDo = false;
      }

      // If it is a DO, and the previous instruction is DONT, then create new range
      else if (dos.contains(i) && !prevIsDo) {
        x = i;
        prevIsDo = true;
      }

      // If it is a don't, but the previous instruction is don't, then continue looping
      // If it is a DO, and the previous instruction is DO, then continue looping
      else { continue; }
    }

    // last range:
    if (prevIsDo && x != 0) {
      l.add(new Pair<>(x, n));
    }

    return l;
  }

  boolean isOk(int idx, List<Pair<Integer, Integer>> ps) {
    for (Pair<Integer, Integer> p : ps) {
      if (p.getA() < idx && idx < p.getB()) {;
        return true;
      }
    }
    return false;
  }

  private void part2(String memory) throws IOException {
    List<Pair<Integer, String>> matches = findMatchesWithIndex("mul\\((\\d*),(\\d*)\\)", memory);
    List<Integer> donts = findMatchesWithIndex("don't\\(\\)", memory).stream().map(p -> p.getA()).collect(Collectors.toList());
    List<Integer> dos = findMatchesWithIndex("do\\(\\)", memory).stream().map(p -> p.getA()).collect(Collectors.toList());
    List<Pair<Integer, Integer>> range = enabledRange(memory.length(), donts, dos);
    long ans = matches.stream().filter(match -> {
      return isOk(match.getA(), range);
    }).map(p -> p.getB()).mapToLong(instruction -> {
      int commaIndex = instruction.indexOf(',');
      long a = Long.parseLong(instruction.substring(4, commaIndex));
      long b = Long.parseLong(instruction.substring(commaIndex + 1, instruction.length() - 1));
      return a * b;
    }).sum();

    w.write("Part 2: " + ans + "\n");
    w.flush();
  }

  private void run() throws IOException {
    StringBuffer sb = new StringBuffer();
    String line;
    while ((line = r.readLine()) != null) {
      if (line.equals("")) {
        break;
      }
      sb.append(line);
    }

    String memory = sb.toString();
    part1(memory);
    part2(memory);
  }

  public static void main(String[] a) throws Exception {
    (new template()).run();
  }
}

class Pair<A, B> {
  A a;
  B b;

  Pair(A a, B b) {
    this.a = a;
    this.b = b;
  }

  public A getA() { return a; }
  public B getB() { return b; }
  public void setA(A a) { this.a = a; }
  public void setB(B b) { this.b = b; }

  @Override
  public String toString() {
    return "(a=" + getA() + ",b=" + getB() + ")";
  }
}
