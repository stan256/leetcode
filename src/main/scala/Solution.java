import java.util.Arrays;
import java.util.List;

public class Solution {

    private static class Node {
        private int x;
        private int y;

        Node(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }

    // google foobar challenge #2 - Don't Get Volunteered!
    public static int solution(int src, int dest) {
        Node source = new Node(src % 8, src / 8);
        Node destination = new Node(dest % 8, dest / 8);
        return subSolution(source, destination);
    }

    static int subSolution(Node source, Node destination) {
        int xDiff = destination.x - source.x;
        int yDiff = destination.y - source.y;
        int xAbs = Math.abs(xDiff);
        int yAbs = Math.abs(yDiff);

        if (source.x == destination.x && source.y == destination.y)
            return 0;
        boolean oneStepFromKnight = (xAbs == 1 && yAbs == 2) || (xAbs == 2 && yAbs == 1);
        boolean twoStepsFromKnight = (xAbs == 2 && yAbs == 0) || (xAbs == 0 && yAbs == 2) || (xAbs == 1 && yAbs == 1);
        boolean threeStepsFromKnight = (xAbs == 1 && yAbs == 0) || (xAbs == 0 && yAbs == 1) || (xAbs == 3 && yAbs == 0) || (xAbs == 0 && yAbs == 3);

        if (!(oneStepFromKnight || twoStepsFromKnight || threeStepsFromKnight)) {
            List<Node> possibleSteps = Arrays.asList(
                    new Node(xDiff < 0 ? source.x - 2 : source.x + 2, yDiff < 0 ? source.y - 1 : source.y + 1),
                    new Node(xDiff < 0 ? source.x - 1 : source.x + 1, yDiff < 0 ? source.y - 2 : source.y + 2)
            );

            return possibleSteps.stream().map(step -> subSolution(step, destination) + 1).min(Integer::compare).get();
        } else {
            if (oneStepFromKnight) return 1;
            else if (twoStepsFromKnight) return 2;
            else return 3;
        }
    }



    public static void main(String[] args) {

//        System.out.println(Solution.solution(29, 0));
        System.out.println(Solution.solution(29, 1));
//        System.out.println(Solution.solution(29, 8));
//        System.out.println(Solution.solution(29, 16));
    }
}
