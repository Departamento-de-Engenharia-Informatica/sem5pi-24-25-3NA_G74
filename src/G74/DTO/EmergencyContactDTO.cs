using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.DTO;

public class EmergencyContactDTO
{
    public string Name { get; set; }
    public string PhoneNumber { get; set; }

    public EmergencyContactDTO(string name, string phoneNumber)
    {
        Name = name;

        PhoneNumber = phoneNumber;
    }
}