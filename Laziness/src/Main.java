/**
 * Created by brandt on 10/26/15.
 */
public class Main {

    public static void main(String[] args) throws Exception {

        new Main().testEvaluation();

    }

    private class EagerObject {
        EagerObject() throws Exception {
            throw new Exception("This wouldn't happen if the language was lazy.");
        }
    }

    public void testEvaluation() throws Exception {

        EagerObject obj = new EagerObject();
        System.out.println("We totally are lazy!");
    }


    // === Some stuff I was messing around with...

//    private void testConstructor() throws Exception {
//        EagerObject obj = new EagerObject();
//    }

//    private void testEvaluation() throws Exception {
//        successIfLazy(lazyCreate());
//    }
//
//    private void successIfLazy(EagerObject lazyTest) {
//        System.out.println("We totally are lazy!");
//    }
//
//    private EagerObject lazyCreate() throws Exception {
//        return new EagerObject();
//    }

}
