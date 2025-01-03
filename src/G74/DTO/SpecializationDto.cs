using G74.Domain.Aggregates.Specialization;

namespace G74.DTO;

public class SpecializationDto
{
    public long Code { get; set; }
    public string Designation { get; set; }
    
    // Constructor for creating from domain model (e.g., for API response)
    public static SpecializationDto FromDomain(Specialization specialization)
    {
        return new SpecializationDto
        {
            Code = specialization.Code.Value,
            Designation = specialization.Designation.Value
        };
    }
    
    
    
    static public IEnumerable<SpecializationDto> FromDomain(IEnumerable<Specialization> specializations)
    {
        List<SpecializationDto> specializationsDto = new List<SpecializationDto>();

        foreach (Specialization specialization in specializations)
        {
            SpecializationDto specializationDto = FromDomain(specialization);

            specializationsDto.Add(specializationDto);
        }
        return specializationsDto;
    }
}