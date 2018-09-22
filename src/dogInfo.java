import java.io.Serializable;
import java.beans.*;


public class dogInfo implements Serializable {
	
	private static final long serialVersionUID = 1L;
	
	public int age=0;
	public String breedSize="";
	public int weight =0;
	public String deworm ="";
	public String vaccine = "";

	public dogInfo(){
		super();
	}
	
	// property change listener for shadow fact value changes
private PropertyChangeSupport pcs = new PropertyChangeSupport(this);
	
	public void addPropertyChangeListener(PropertyChangeListener pcl) {
		pcs.addPropertyChangeListener(pcl);
	}
	public void removePropertyChangeListener(PropertyChangeListener pcl) {
		pcs.removePropertyChangeListener(pcl);
	}
	
	public int getAge(){
		return age;
	}
	public void setAge(int age) {
		int temp = this.age;
		this.age = age;
		pcs.firePropertyChange("age", new Integer(temp), new Integer(age));
		
	}
	
	
	
	public int getWeight(){
		return weight;
	}
	
	public void setWeight(int weight) {
		int temp = this.weight;
		this.weight = weight;
		pcs.firePropertyChange("weight", new Integer(temp), new Integer(weight));
		
	}
	
	public String getBreedSize(){
		return breedSize;
	}
	public void setBreedSize(String breedSize) {
		String temp = this.breedSize;
		this.breedSize = breedSize;
		pcs.firePropertyChange("breedSize", new String(temp), new String(breedSize));
	}
	
	public String getDeworm(){
		return deworm;
	}
	public void setDeworm(String deworm) {
		String temp = this.deworm;
		this.deworm = deworm;
		pcs.firePropertyChange("deworm", new String(temp), new String(deworm));
	}
	
	public String getVaccine(){
		return vaccine;
	}
	public void setVaccine(String vaccine) {
		String temp = this.vaccine;
		this.vaccine = vaccine;
		pcs.firePropertyChange("vaccine", new String(temp), new String(vaccine));
	}
	
	
}
