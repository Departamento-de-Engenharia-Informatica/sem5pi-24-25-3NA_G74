using G74.Domain.Shared;

public class LicenceNumber : IValueObject
{
    public long licenceNumber;

    public LicenceNumber(long licenceNumber)
    {
        this.licenceNumber = licenceNumber;
    }

    public long number { get; private set; }

}