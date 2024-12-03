import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

class template {
  BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
  BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));

  private boolean isVowel(char c) {
    return c == 'a' || c == 'i' || c == 'u' || c == 'e' || c == 'o';
  }

  private boolean atLeastThreeVowels(String s) {
    int count = 0;
    for (char c : s.toCharArray()) {
      if (isVowel(c)) {
        count += 1;
      }
    }
    return count >= 3;
  }

  private boolean atLeastOneLetterAppearingTwice(String s) {
    for (int i = 0; i < s.length() - 1; i++) {
      if (s.charAt(i) == s.charAt(i+1)) {
        return true;
      }
    }
    return false;
  }

  private boolean noBadStrings(String s) {
    return ! (s.contains("ab") || s.contains("cd") || s.contains("pq") || s.contains("xy"));
  }

  private void part1(List<String> lines) throws IOException {
    int ans = 0;
    for (String line : lines) {
      if (atLeastThreeVowels(line) && atLeastOneLetterAppearingTwice(line) && noBadStrings(line)) {
        ans += 1;
      }
    }

    w.write("Part 1: " + ans + "\n");
    w.flush();
  }

  private boolean pairAppearsTwice(String s) throws IOException {
    for (int i = 2; i < s.length() - 1; i++) {
      String pair = s.substring(i-2, i);
      String rest = s.substring(i);

      if (rest.contains(pair)) {
        return true;
      }
    }
    return false;
  }

  private boolean hasRepeatingPattern(String s) throws IOException {
    for (int i = 0; i < s.length() - 2; i++) {
      if (s.charAt(i+2) == s.charAt(i)) {
        return true;
      }
    }
    return false;
  }

  private void part2(List<String> lines) throws IOException {
    int ans = 0;
    for (String line : lines) {
      if (pairAppearsTwice(line) && hasRepeatingPattern(line)) {
        ans += 1;
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

  public static void main(String[] a) throws Exception {
    (new template()).run();
  }

}
