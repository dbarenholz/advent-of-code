import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class template {
  BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
  BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));

  private boolean orderOk(Map<Long, List<Long>> sortingRules , List<Long> nums) {
    for (int i = 0; i < nums.size(); i ++) {
      Long val = nums.get(i);
      List<Long> remaining = new ArrayList<>();
      for (int j = i + 1; j < nums.size(); j++) {
        remaining.add(nums.get(j));
      }

      List<Long> invalid = sortingRules.getOrDefault(val, List.of());

      for (Long iCantExist : invalid) {
        if (remaining.contains(iCantExist)) {
          return false;
        }
      }
    }

    return true;
  }


  // INFO: entry k: [v] means that for 'k', each 'v' must come before it
  private Map<Long, List<Long>> getMapForLines(List<String> lines) {
    Map<Long, List<Long>> mp = new HashMap<>();
    for (String line : lines) {
      String[] tokens = line.split("\\|");
      Long X = Long.parseLong(tokens[0]);
      Long Y = Long.parseLong(tokens[1]);

      if (mp.containsKey(Y)) {
        List<Long> addX = new ArrayList<>();
        addX.addAll(mp.get(Y));
        addX.add(X);
        mp.put(Y, addX);
      } else {
        mp.put(Y, List.of(X));
      }
    }

    return mp;
  }

  private List<List<Long>> getNumsForLines(List<String> numLines) {
    List<List<Long>> nums = new ArrayList<>();
    for (String s : numLines) {
      String[] tokens = s.split(",");

      List<Long> vals = new ArrayList<>();
      for (String t : tokens) {
        vals.add(Long.parseLong(t));
      }

      nums.add(vals);
    }

    return nums;

  }

  private Long middleOf(List<Long> nums) {
    return nums.get( (nums.size() - 1) / 2 );
  }

  private void part1(List<String> sortingLines, List<String> numLines) throws IOException {
    long ans = 0L;

    Map<Long, List<Long>> mp = getMapForLines(sortingLines);
    List<List<Long>> manyNums = getNumsForLines(numLines);

    for (List<Long> nums : manyNums) {
      if (orderOk(mp, nums)) {
        ans += middleOf(nums);
      }
    }

    w.write("Part 1: " + ans + "\n");
    w.flush();
  }

  private List<Integer> violatingIndices(Map<Long, List<Long>> sortingRules, List<Long> nums) {
    for (int i = 0; i < nums.size(); i ++) {
      Long val = nums.get(i);
      List<Long> remaining = new ArrayList<>();
      for (int j = i + 1; j < nums.size(); j++) {
        remaining.add(nums.get(j));
      }

      List<Long> invalid = sortingRules.getOrDefault(val, List.of());

      for (int j = 0; j < invalid.size(); j ++) {
        if (remaining.contains(invalid.get(j))) {
          return List.of(i, nums.indexOf(invalid.get(j)));
        }
      }
    }

    return List.of();
  }

  private List<Long> reorder(Map<Long, List<Long>> sortingRules, List<Long> nums) {
    List<Long> theNums = new ArrayList<>(nums);

    while (!orderOk(sortingRules, theNums)) {
      // where is my object destructuring to assign (i, j) = violatingIndices :')
      List<Integer> indices = violatingIndices(sortingRules, theNums);

      int i = indices.get(0);
      int j = indices.get(1);

      // where is my lst.swap(i, j) :')
      long tmp = theNums.get(i);
      theNums.set(i, theNums.get(j));
      theNums.set(j, tmp);
    }

    return theNums;
  }

  private void part2(List<String> sortingLines, List<String> numLines) throws IOException {
    long ans = 0L;

    Map<Long, List<Long>> mp = getMapForLines(sortingLines);
    List<List<Long>> manyNums = getNumsForLines(numLines);

    for (List<Long> nums : manyNums) {
      if (!orderOk(mp, nums)) {
        ans += middleOf(reorder(mp, nums));
      }
    }

    w.write("Part 2: " + ans + "\n");
    w.flush();
  }

  private void run() throws IOException {
    List<String> sortingLines = new ArrayList<>();
    List<String> nums = new ArrayList<>();
    String line;

    // Read sortingLines
    while ((line = r.readLine()) != null) {
      if (line.equals("")) {
        break;
      }
      sortingLines.add(line);
    }

    // Read rawNums
    while ((line = r.readLine()) != null) {
      if (line.equals("")) {
        break;
      }
      nums.add(line);
    }

    part1(sortingLines, nums);
    part2(sortingLines, nums);
  }

  public static void main(String[] a) throws Exception {
    (new template()).run();
  }
}
