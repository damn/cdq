package game.tools;

/**
 * Counts from 0 to maxTime in ms Can tell when time is reached. TODO
 * countIfActive gibt true/false zurück falls finished.
 *
 * @author Michi
 *
 */
public class Counter {
	private int		maxCount;
	private boolean	active;
	private int		currentCount;

	/**
	 * Starts active!
	 *
	 * @param maxTime
	 *            maximum time in ms
	 */
	public Counter(int maxTime) {
		this.maxCount = maxTime;
		restart();
	}

	/**
	 * Creates an inactive counter with maxtime=0 and currentcount=0
	 */
	public Counter() {
		this.active = false;
	}

	/**
	 * current time of counter
	 *
	 * @return
	 */
	public int getCurrentCount() {
		return currentCount;
	}

	/**
	 * maxWert erreicht?
	 *
	 * @return
	 */
	public boolean isFinished() {
		if (currentCount >= maxCount) {
			return true;
		} else {
			return false;
		}
	}

	public boolean isFinishedAndActive() {
		return (isFinished() && isActive());
	}

	/**
	 * setz auf 0
	 */
	public void setCountToZero() {
		currentCount = 0;
	}

	public void setActive(boolean active) {
		this.active = active;
	}

	public void setMax(int time) {
		maxCount = time;

	}

	/**
	 * zähl hoch FALLS aktiv
	 *
	 * @param delta
	 *            in ms
	 */
	public void countIfActive(int delta) {
		if (active) {
			currentCount += delta;
		}

	}

	public boolean isActive() {
		return active;
	}

	/**
	 * between 0.0f and 1.0f
	 *
	 * @return
	 */
	public float getProgress() {
		float progress = 0;
		if (maxCount != 0) {
			progress = (float) currentCount / (float) maxCount;
		}

		return progress;
	}

	/**
	 * Inaktiv und count auf 0 setzen.
	 */
	public void stop() {
		setActive(false);
		setCountToZero();

	}

	/**
	 * Aktiv und count auf 0 setzen.
	 */
	public void restart() {
		setActive(true);
		setCountToZero();

	}

	public int getMax() {
		return maxCount;
	}

}
