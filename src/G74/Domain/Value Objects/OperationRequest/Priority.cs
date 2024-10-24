using G74.Domain.Shared;



public class Priority : IValueObject
{
   public enum PriorityType{
        ElectiveSurgery,
        UrgentSurgery,
        EmergencySurgery
   } 
   public PriorityType PriorityDescription { get; private set;}

   public Priority(PriorityType type){
       PriorityDescription = type;
   }

    public Priority(string description){
        PriorityDescription = (PriorityType)Enum.Parse(typeof(PriorityType), description);
    }
   
}

