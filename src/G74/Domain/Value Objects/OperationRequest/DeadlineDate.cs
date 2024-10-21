using G74.Domain.Shared;

public class DeadlineDate : IValueObject
{
    public DateTime date { get; private set; }
}