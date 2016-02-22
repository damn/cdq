package game.tools;

import java.io.File;
import java.io.IOException;

import org.newdawn.slick.AppGameContainer;
import org.newdawn.slick.BasicGame;
import org.newdawn.slick.Color;
import org.newdawn.slick.GameContainer;
import org.newdawn.slick.Graphics;
import org.newdawn.slick.Image;
import org.newdawn.slick.Input;
import org.newdawn.slick.SlickException;
import org.newdawn.slick.imageout.ImageOut;
import org.newdawn.slick.particles.ParticleIO;
import org.newdawn.slick.particles.ParticleSystem;

// http://slick.javaunlimited.net/viewtopic.php?t=718&highlight=particlesystem
// Sprite- Packer max size 2048*2048 -> Anim_duration/shot_update_interval frames * WIDTH * HEIGHT < 2048*2048 sein muss! 

public class ParticleSystemToImage extends BasicGame {

	private static final int		SHOT_UPDATE_INTERVAL	= 33;
	private final static int		DONT_TAKE_FIRST_N_SHOTS	= 0;
	private static final int		ANIMATION_DURATION		= 400
																	+ DONT_TAKE_FIRST_N_SHOTS
																	* SHOT_UPDATE_INTERVAL;

	private static final int		SIZE					= 400;
	private static final int		WIDTH					= SIZE;
	private static final int		HEIGHT					= SIZE;

	private final static String		SYSTEMS_FOLDER			= "e:/my dropbox/dev/pedigreesystems/new/";

	private final static String		particlesystem_filename	= "bossexplosion";

	private final static String		PREFIX					= "";
	private final static String		ANIMATIONS_FOLDER		= "particleanimation/";
	private final static String		TARGET_FOLDER			= ANIMATIONS_FOLDER
																	+ particlesystem_filename
																	+ "/";
	private final static boolean	WRITE_ALPHA				= true;

	private int						xStart					= 250;
	private int						yStart					= 0;
	private int						y;
	private int						x;

	private int						updateCount;
	private GameContainer			container;
	private ParticleSystem			particleSystem;
	private Graphics				g;
	private Image					copy;
	private Counter					durationCounter;
	private Counter					closeCounter;
	private int						screenshotcounter;

	public ParticleSystemToImage() {
		super("ParticleSystemToImage");
	}

	public void init(GameContainer container) throws SlickException {
		this.container = container;
		this.g = container.getGraphics();

		try {
			particleSystem = ParticleIO.loadConfiguredSystem(SYSTEMS_FOLDER
					+ particlesystem_filename + ".xml");
		} catch (IOException e) {
			throw new SlickException("Failed to load particle systems", e);
		}

		copy = new Image(WIDTH, HEIGHT);

		durationCounter = new Counter(ANIMATION_DURATION);

		closeCounter = new Counter(1000);

		if ((new File(ANIMATIONS_FOLDER)).mkdir()) {
			System.out.println("Created folder: " + ANIMATIONS_FOLDER);
		}

		File targetFolder = new File(TARGET_FOLDER);
		if (targetFolder.mkdir()) {
			System.out.println("Created folder: " + TARGET_FOLDER);
		}

		for (File file : targetFolder.listFiles()) {
			file.delete();
		}

	}

	public void render(GameContainer container, Graphics g) {
		g.setColor(Color.white);
		g.drawRect(xStart - 1, yStart - 1, WIDTH + 2, HEIGHT + 2);

		g.setColor(Color.white);
		g.drawString("Mouse in Shot:" + x + "/" + y, 0, 40);

		if (!durationCounter.isActive()) {
			g.setColor(Color.red);
			g.drawString("Finished with " + screenshotcounter + " Frames.", 0,
					80);
		}

		g.translate(xStart + WIDTH / 2, yStart + HEIGHT / 2);
		particleSystem.render();
	}

	private static String createNumberString(int n) {
		String number = "";
		if (n < 10) {
			number = "0" + n;
		} else {
			number = String.valueOf(n);
		}
		return number;
	}

	private void takeShot() throws SlickException {
		g.copyArea(copy, xStart, yStart);
		// copy = copy.getFlippedCopy(false, true);
		String no = createNumberString(screenshotcounter);
		ImageOut.write(copy, TARGET_FOLDER + PREFIX + no + ".png", WRITE_ALPHA);

		screenshotcounter++;
		copy = new Image(WIDTH, HEIGHT);
	}

	public void update(GameContainer container, int delta)
			throws SlickException {

		Input i = container.getInput();

		x = (i.getMouseX() - xStart);
		y = (i.getMouseY() - yStart);

		particleSystem.update(SHOT_UPDATE_INTERVAL);
		durationCounter.countIfActive(SHOT_UPDATE_INTERVAL);

		if (durationCounter.isActive() && (updateCount > 0)
				&& (updateCount > DONT_TAKE_FIRST_N_SHOTS)) {
			takeShot();
		}

		if (durationCounter.isFinished()) {
			durationCounter.setActive(false);
		}

		if (!durationCounter.isActive()) {
			closeCounter.countIfActive(delta);
			if (closeCounter.isFinished()) {
				container.exit();
			}
		}

		updateCount++;

	}

	public static void main(String[] argv) {
		try {
			AppGameContainer container = new AppGameContainer(
					new ParticleSystemToImage());
			container.setDisplayMode(800, 600, false);

			container.start();
		} catch (SlickException e) {
			e.printStackTrace();
		}
	}

	public void keyPressed(int key, char c) {
		if (key == Input.KEY_ESCAPE) {
			container.exit();
		}
	}
}
