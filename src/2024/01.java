import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

class template {
  BufferedReader r = new BufferedReader(new InputStreamReader(System.in));
  BufferedWriter w = new BufferedWriter(new OutputStreamWriter(System.out));

  // Part 1: compute the distance between sorted lists.
  private Long distance(List<Long> sortedA, List<Long> sortedB) throws Exception {
    int n = sortedA.size();
    if (n != sortedB.size()) {
      throw new Exception("Lists should be equal length");
    }

    long sum = 0L;

    for (int i = 0; i < n; i ++) {
      long a = sortedA.get(i);
      long b = sortedB.get(i);

      sum += Math.abs(a - b);
    }

    return sum;
  }

  // Part 2: compute the similarity according to specification
  private Long similarity(List<Long> nums, Map<Long, Long> mp) {
    return nums.stream()
      .mapToLong(num -> {
        Long mult = mp.get(num) == null ? 0 : mp.get(num);
        return num * mult;
      })
      .sum();
  }

  // run is used in both parts -- some work might be unnecessary depending on the part.
  private void run() throws Exception {
    List<Long> a = new ArrayList<>();
    List<Long> b = new ArrayList<>();

    String line;
    while ((line = r.readLine()) != null) {
      if (line.equals("")) {
        break;
      }
      String[] tokens = line.stripLeading().stripTrailing().split("   ");
      a.add(Long.parseLong(tokens[0]));
      b.add(Long.parseLong(tokens[1]));
    }

    // Part 1 only
    // Collections.sort(a);
    // Collections.sort(b);
    // w.write(distance(a, b) + "\n");


    // Part 2 only
    Map<Long, Long> mp = b.stream().collect(Collectors.groupingBy(x -> x, Collectors.counting()));
    w.write(similarity(a, mp) + "\n");

    // Always flush the toilet
    w.flush();
  }

  public static void main(String[] a) throws Exception {
    (new template()).run();
  }

}
