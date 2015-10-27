/**
 * Created by brandt on 10/26/15.
 */
public class Main {

    public static void main(String[] args) throws Exception {

        // we should change these names later--I did long names to make it clearer to understand
        new Main().testEvaluationByParamAndConstructor();
        new Main().testEvaluationOnlyByConstructor();
    }

    private class EagerObject {
        EagerObject() throws Exception {
            throw new Exception("This wouldn't happen if the language was lazy.");
        }
    }

    //=========== He talked about needing to show laziness by parameter passing,
    //=========== so I don't think this will work... The other functions might be better?
    public void testEvaluationOnlyByConstructor() throws Exception {

        EagerObject obj = new EagerObject();
        System.out.println("We totally are lazy!");
    }

    private void testEvaluationByParamAndConstructor() throws Exception {
        successIfLazy(new EagerObject());
    }

    private void successIfLazy(EagerObject lazyTest) {
        System.out.println("We totally are lazy!");
    }

    // === Some stuff I was messing around with...

//    private void testConstructor() throws Exception {
//        EagerObject obj = new EagerObject();
//    }

//    private EagerObject lazyCreate() throws Exception {
//        return new EagerObject();
//    }

}
