package ex1;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

public class ex1 {

    public ex1() {

    }

    public static void main(String[] args) {

        ex1 test = new ex1();

        try {
            FileInputStream inputStream = new FileInputStream("ex1/ndata.dat");
            //FileInputStream inputStream = new FileInputStream("ex1/test.dat");

            Bitnode rootNode = test.new Bitnode();
            Bitnode curr = rootNode;
            byte[] byteArray = new byte[4];

            while(inputStream.read(byteArray, 0, 4) != -1) {
                curr = rootNode;
                for(int i = 3; i >= 0; i--) {
                    for(int j = 7; j >= 0; j--) {
                        if(0 == (byteArray[i] >> j & 1)) {
                            if(curr.hasLeftChild()) {
                                curr = curr.getLeftChild();
                            }
                            else {
                                Bitnode newLeftChild = test.new Bitnode();
                                curr.setLeftChild(newLeftChild);
                                curr = newLeftChild;
                            }
                        }
                        else {
                            if(curr.hasRightChild()) {
                                curr = curr.getRightChild();
                            }
                            else {
                                Bitnode newRightChild = test.new Bitnode();
                                curr.setRightChild(newRightChild);
                                curr = newRightChild;
                            }
                        }
                    }
                }
            }
            inputStream.close();

            if(rootNode.hasLeftChild()) {
                printNumbersSubtreeLeft(rootNode.getLeftChild(), 0, -2147483648);
            }
            if(rootNode.hasRightChild()) {
                printNumbersSubtreeRight(rootNode.getRightChild(), 0, -2147483648);
            }

        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        catch (IOException e) {
            e.printStackTrace();
        }

    }
    
    private static void printNumbersSubtreeRight(Bitnode currNode, int value,
        int power) {

        value -= power;

        if(!currNode.hasLeftChild() && currNode.hasRightChild()) {
            System.out.println(value);
            return;
        }
        if(currNode.hasLeftChild()) {
            printNumbersSubtreeLeft(currNode.getLeftChild(), value, power / 2);
        }
        if(currNode.hasRightChild()) {
            printNumbersSubtreeRight(currNode.getRightChild(), value, power / 2);
        }
    }

    private static void printNumbersSubtreeLeft(Bitnode currNode, int value,
        int power) {

        if(!currNode.hasLeftChild() && currNode.hasRightChild()) {
            System.out.println(value);
            return;
        }
        if(currNode.hasLeftChild()) {
            printNumbersSubtreeLeft(currNode.getLeftChild(), value, power / 2);
        }
        if(currNode.hasRightChild()) {
            printNumbersSubtreeRight(currNode.getRightChild(), value, power / 2);
        }

    }

    public class Bitnode {

        private Bitnode _leftChild;

        private Bitnode _rightChild;

        public Bitnode() {

            _leftChild = null;
            _rightChild = null;
        }

        public boolean hasLeftChild() {

            return !(_leftChild == null);

        }

        public boolean hasRightChild() {

            return !(_rightChild == null);

        }

        public Bitnode getRightChild() {

            return _rightChild;
        }

        public void setRightChild(Bitnode rightChild) {

            _rightChild = rightChild;
        }

        public Bitnode getLeftChild() {

            return _leftChild;
        }

        public void setLeftChild(Bitnode leftChild) {

            _leftChild = leftChild;
        }
    }
}
