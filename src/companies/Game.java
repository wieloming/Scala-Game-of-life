package companies;

import javax.swing.*;

public class Game {
    public static void main(String[] args){
        JFrame window = new JFrame("Game of Life");


        GamePanel panel = new GamePanel();
        Thread t = new Thread(panel);
        t.start();

        window.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        window.setContentPane(panel);
        window.setResizable(false);
        window.setVisible(true);
        window.setAlwaysOnTop(true);
        window.pack();
    }
}
