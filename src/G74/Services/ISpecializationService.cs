using G74.DTO;

namespace G74.Services;

public interface ISpecializationService
{
    Task<IEnumerable<SpecializationDto>> GetAll();

    Task<SpecializationDto?> GetByCode(long code);

    Task<SpecializationDto> Add(SpecializationDto specializationDto);
}