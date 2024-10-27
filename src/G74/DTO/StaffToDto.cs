namespace G74.DTO;

public class StaffToDto
{
    public StaffDto JsonToDto(JsonStaffDto jsonStaffDto)
    {
        StaffDto staffDto = new StaffDto(jsonStaffDto.LicenseNumber, jsonStaffDto.Name,
            jsonStaffDto.PhoneNumber, jsonStaffDto.ContactEmail, jsonStaffDto.StaffSpecialization,
            jsonStaffDto.Status);
        return staffDto;
    }
}