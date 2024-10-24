using G74.Domain.Shared;

public class LicenceNumber : IValueObject
{
    public string licenceNumber;

    public LicenceNumber(string licenceNumber)
    {
        this.licenceNumber = licenceNumber;
    }

    public string number { get; private set; }

}