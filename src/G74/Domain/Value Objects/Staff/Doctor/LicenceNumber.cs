using G74.Domain.Shared;

public class LicenceNumber : IValueObject
{
    public long licenceNumber;

    public LicenceNumber(long licenceNumber)
    {
        this.licenceNumber = licenceNumber;
    }
    public LicenceNumber(string licenceNumber)
    {
        try
        {
            this.licenceNumber = long.Parse(licenceNumber);
        }
        catch (System.Exception)
        {
            throw new System.ArgumentException("Invalid Licence Number");
        }
    
    }

    public override string ToString()
    {
        return licenceNumber.ToString();
    }

}