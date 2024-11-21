namespace G74.DTO;

public class ContactInformationDTO
{
    
    
    public string? PhoneNumber { get; init; }

    public string? EmailAddress { get; init; }

    public ContactInformationDTO()
    {
        
    }
    
    public ContactInformationDTO(string? phoneNumber = null, string? emailAddress = null)
    {
        PhoneNumber = phoneNumber;
        EmailAddress = emailAddress;
    }
}