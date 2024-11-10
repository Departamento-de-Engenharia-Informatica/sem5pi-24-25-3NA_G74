using G74.Domain.Shared;

public class Duration : IValueObject
{
    public int DurationTime { get; private set; }

    public Duration(int Duration){
        this.DurationTime = Duration;
        
    }
    
}