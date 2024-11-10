using G74.Adapters.Repositories;
using G74.Domain.IRepositories;
using G74.DTO;

namespace G74.Services;

public class OptimizationModuleService
{
    private readonly IRepoUser _repoUser;
    private readonly IOperationRequestRepository _operationRequestRepository;
    private readonly IOperationTypeRepository _operationTypeRepository;
    private readonly IPatientRepository _patientRepository;
    private readonly IStaffRepository _staffRepository;
    private readonly ISurgeryRoomRepository _surgeryRoomRepository;
    private readonly IAppointmentRepository _appointmentRepository;
    private readonly IConfiguration _configuration;

    public OptimizationModuleService(
        IRepoUser repoUser, 
        IConfiguration configuration,
        IOperationRequestRepository operationRequestRepository,
        IOperationTypeRepository operationTypeRepository,
        IPatientRepository patientRepository,
        IStaffRepository staffRepository,
        ISurgeryRoomRepository surgeryRoomRepository,
        IAppointmentRepository appointmentRepository
        )
    {
        _repoUser = repoUser;
        _operationRequestRepository = operationRequestRepository;
        _operationTypeRepository = operationTypeRepository;
        _patientRepository = patientRepository;
        _staffRepository = staffRepository;
        _surgeryRoomRepository = surgeryRoomRepository;
        _appointmentRepository = appointmentRepository;
        _configuration = configuration;
    }
    
    public async Task ExportAllDataToProlog()
    {
        await _staffRepository.ExportStaffDataToProlog();
    }

}