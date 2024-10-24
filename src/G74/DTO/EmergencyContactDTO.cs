namespace G74.DTO;

public class EmergencyContactDTO
{
    public string PhoneNumber { get; init; }

    public EmergencyContactDTO(string phoneNumber)
    {
        PhoneNumber = phoneNumber;
    }
}