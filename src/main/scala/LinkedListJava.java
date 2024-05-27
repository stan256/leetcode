public class LinkedListJava {
    public static class ListNode {
        int val;
        ListNode next;

        ListNode() {
        }

        ListNode(int val) {
            this.val = val;
        }

        ListNode(int val, ListNode next) {
            this.val = val;
            this.next = next;
        }
    }

    public ListNode addTwoNumbers(ListNode l1, ListNode l2) {
        int v1 = l1.val;
        int v2 = l2.val;
        boolean addOne = v1 + v2 > 9;
        ListNode result = new ListNode((v1 + v2) % 10);
        ListNode current = result;
        ListNode i1 = l1;
        ListNode i2 = l2;

        while (i1.next != null || i2.next != null) {
            if (i1.next != null) {
                i1 = i1.next;
                v1 = i1.val;
            } else {
                v1 = -1;
            }

            if (i2.next != null) {
                i2 = i2.next;
                v2 = i2.val;
            } else {
                v2 = -1;
            }

            int nextValue;
            if (v1 < 0 || v2 < 0) nextValue = v1 + v2 + 1;
            else nextValue = v1 + v2;

            if (addOne) {
                nextValue += 1;
                addOne = false;
            }

            if (nextValue > 9) {
                addOne = true;
                nextValue %= 10;
            }

            current.next = new ListNode(nextValue);
            current = current.next;
        }

        if (addOne) current.next = new ListNode(1);

        return result;
    }

    public static void main(String[] args) {
        ListNode l1 = new ListNode(9, new ListNode(9, new ListNode(9, new ListNode( 9, new ListNode(9, new ListNode(9, new ListNode(9)))))));
        ListNode l2 = new ListNode( 9, new ListNode(9, new ListNode(9, new ListNode(9))));
        ListNode x = new LinkedListJava().addTwoNumbers(l1, l2);

        while (x != null) {
            System.out.println(x.val);
            x = x.next;
        }
    }
}
