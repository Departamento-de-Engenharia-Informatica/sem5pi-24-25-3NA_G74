using G74.Domain.Shared;

public class Duration : IValueObject
{
    public int days { get; private set; }
    public int hours { get; private set; }
    public int minutes { get; private set; }
    public int seconds { get; private set; }
    
}