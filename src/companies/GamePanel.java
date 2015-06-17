package companies;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;

public class GamePanel extends JPanel implements Runnable {
    public static final int WIDTH = 100;
    public static final int HEIGHT = 50;
    private final int SCALE = 15;
    private int FPS = 250;
    private long optimalTime = 20000 / FPS;
    private boolean running = false;
    private Graphics2D g;
    private Engine engine;
    private Engine.Board board;

    public GamePanel() {
        super();
        setPreferredSize(new Dimension(WIDTH * SCALE, HEIGHT * SCALE));
        setMinimumSize(new Dimension(WIDTH, HEIGHT));
        setFocusable(true);
        requestFocus();
    }

    public void run() {
        init();
        long startTime;
        long elapsedTime;
        long waitTime;
        while (running) {
            startTime = System.nanoTime();
            update();
            repaint();
            elapsedTime = System.nanoTime() - startTime;
            waitTime = optimalTime - elapsedTime / 1000000;
            try {
                if (waitTime <= 0) {
                    waitTime = 2;
                }
                Thread.sleep(waitTime);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }

        }
    }

    private void init() {
        BufferedImage image = new BufferedImage(WIDTH * SCALE, HEIGHT * SCALE, BufferedImage.TYPE_INT_RGB);
        g = (Graphics2D) image.getGraphics();
        engine = new Engine();
        board = engine.createBoard(WIDTH, HEIGHT);
        running = true;
    }


    private void update() {
        board = engine.tick(board);
    }

    public void paintComponent(Graphics g) {
        super.paintComponent(g);

        g.setColor(Color.BLACK);
        g.fillRect(0, 0, WIDTH * SCALE, HEIGHT * SCALE);
        if (running) {
            drawBoard(g);
        }
    }

    private void drawBoard(Graphics g) {
        g.setColor(Color.red);
        int rowNum = 0;
        int[][] arrayBoard = board.toArray();
        for (int[] row : arrayBoard) {
            int colNum = 0;
            for (int point : row) {
                if(point == 1){
                    g.fillRect(colNum * SCALE, rowNum * SCALE, SCALE, SCALE);
                }
                colNum++;
            }
            rowNum++;
        }
    }

}
