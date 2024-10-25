using G74.Domain.Shared;

public class Duration : IValueObject
{
    public int Days { get; private set; }
    public int Hours { get; private set; }
    public int Minutes { get; private set; }
    public int Seconds { get; private set; }

    public Duration(int seconds, int minutes, int hours, int days){
        this.Seconds = seconds;
        this.Minutes = minutes;
        this.Hours = hours;
        this.Days = days;
    }
    
}