import java.util.Arrays;

public class Java4Test {

    public static int[] productExceptSelf(int[] nums) {
        int n=nums.length;
        int[] left=new int[n];
        int[] right =new int[n];
        int[] ans=new int[n];

        left[0]=1;
        right[n-1]=1;

        for(int i=1;i<n;i++){
            left[i]=left[i-1]*nums[i-1];
        }

        for(int j=n-2;j>=0;j--){
            right[j]=right[j+1]*nums[j+1];
        }

        for(int q=0;q<n;q++){
            ans[q]=left[q]*right[q];
        }

        return ans;

    }

    public static void main(String[] args) {
        System.out.println(Arrays.toString(productExceptSelf(new int[]{1, 2, 3, 4})));
    }
}
