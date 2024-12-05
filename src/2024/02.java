import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

class template {
  BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
  BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));

  boolean strictlyIncreasing(List<Long> nums) {
    for (int i = 0; i < nums.size() - 1; i++) {
      if (nums.get(i) >= nums.get(i+1)) {
        return false;
      }
    }

    return true;
  }

  boolean strictlyDecreasing(List<Long> nums) {
    for (int i = 0; i < nums.size() - 1; i++) {
      if (nums.get(i) <= nums.get(i+1)) {
        return false;
      }
    }

    return true;
  }

  boolean adjacentValuesDifferBy(List<Long> nums, long lower, long upper) {
    for (int i = 0; i < nums.size() - 1; i++) {
      long difference = Math.abs(nums.get(i) - nums.get(i+1));
      if (!(lower <= difference && difference <= upper)) {
        return false;
      }
    }

    return true;
  }

  boolean safe(List<Long> report) {
    if (!strictlyIncreasing(report) && !strictlyDecreasing(report)) {
      return false;
    }

    if (!adjacentValuesDifferBy(report, 1L, 3L)) {
      return false;
    }

    return true;
  }

  boolean problemDampenerSafe(List<Long> report) throws IOException {
    if (safe(report)) {
      return true;
    }

    List<Long> original = new ArrayList<>(report);

    for (int i = 0; i < report.size(); i++) {
      report.remove(i);
      if (safe(report)) {
        return true;
      }
      report = new ArrayList<>(original);
    }

    return false;
  }

  private void part1(List<List<Long>> reports) throws IOException {
    long safesies = 0L;

    for (List<Long> report : reports) {
      if (safe(report)) {
        safesies ++;
      }
    }

    w.write("Part 1: ");
    w.write(safesies + "\n");
    w.flush();

  }

  private void part2(List<List<Long>> reports) throws IOException {
    long safesies = 0L;

    for (List<Long> report : reports) {
      if (problemDampenerSafe(report)) {
        safesies ++;
      }
    }

    w.write("Part 2: ");
    w.write(safesies + "\n");
    w.flush();

  }

  private void run() throws IOException {
    List<List<Long>> reports = new ArrayList<>();

    String line;
    while ((line = r.readLine()) != null) {
      if (line.equals("")) {
        break;
      }
      List<Long> report = Arrays.stream(line.stripLeading().stripTrailing().split(" "))
        .map(Long::parseLong)
        .collect(Collectors.toList());

      reports.add(report);
    }

    part1(reports);
    part2(reports);
  }

  public static void main(String[] a) throws Exception {
    (new template()).run();
  }

}
