using G74.Domain.Shared;

namespace G74.Domain.Value_Objects.Staff.Doctor;

public class LicenceNumber : IValueObject
{
    public long Value { get; }

    public LicenceNumber(long value)
    {
        this.Value = value;
    }
    public LicenceNumber(string licenceNumber)
    {
        try
        {
            this.Value = long.Parse(licenceNumber);
        }
        catch (System.Exception)
        {
            throw new System.ArgumentException("Invalid Licence Number");
        }
    
    }

    public override string ToString()
    {
        return Value.ToString();
    }

}