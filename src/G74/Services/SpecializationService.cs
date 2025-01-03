using G74.Domain.Aggregates.Specialization;
using G74.Domain.IRepositories;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Specialization;
using G74.DTO;

namespace G74.Services;

public class SpecializationService : ISpecializationService
{
    private readonly IUnitOfWork _unitOfWork;
    private readonly ISpecializationRepository _repo;
    
    public SpecializationService(IUnitOfWork unitOfWork, ISpecializationRepository repo)
    {
        _unitOfWork = unitOfWork;
        _repo = repo;
    }
    
    public async Task<IEnumerable<SpecializationDto>> GetAll()
    {
        IEnumerable<Specialization> specialization = await _repo.GetSpecializationAsync();

        IEnumerable<SpecializationDto> specializationDto = SpecializationDto.FromDomain(specialization);

        return specializationDto;
    }

    public async Task<SpecializationDto?> GetByCode(long code)
    {
        Specialization? specialization = await _repo.GetByCode(new Code(code));

        if (specialization != null)
        {
            SpecializationDto specializationDto = SpecializationDto.FromDomain(specialization);
            return specializationDto;
        }
        return null;
    }

    public async Task<SpecializationDto> Add(SpecializationDto specializationDto)
    {
        var existing = await _repo.GetByCode(new Code(specializationDto.Code));
        if (existing != null)
        {
            throw new BusinessRuleValidationException("Specialization with the same code already exists");
        }

        var specialization = Specialization.Create(specializationDto.Code, specializationDto.Designation);


        await this._repo.Add(specialization);
        await this._unitOfWork.CommitAsync();

        return new SpecializationDto
        {
            Code = specialization.Code.Value,
            Designation = specialization.Designation.Value
        };
    }
}