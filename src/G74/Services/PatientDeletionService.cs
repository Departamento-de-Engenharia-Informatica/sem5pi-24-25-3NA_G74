using G74.DataModel;
using G74.Domain.IRepositories;

namespace G74.Services;

public class PatientDeletionService : BackgroundService
{
    
    private readonly IServiceProvider _serviceProvider;

    public PatientDeletionService(IServiceProvider serviceProvider)
    {
        _serviceProvider = serviceProvider;
    }
    
    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        while (!stoppingToken.IsCancellationRequested)
        {
            await Task.Delay(TimeSpan.FromMinutes(1), stoppingToken);
            await DeletePatientsAsync(stoppingToken);
        }
    }
    
    private async Task DeletePatientsAsync(CancellationToken stoppingToken)
    {
        using (var scope = _serviceProvider.CreateScope())
        {
            var patientRepository = scope.ServiceProvider.GetRequiredService<IPatientRepository>();
            List<PatientDataModel> patients = await patientRepository.GetPatientsReadyForDeletion();

            if (patients.Count > 0)
            {
                foreach (var patient in patients)
                {
                    await patientRepository.DeletePatientDefinitive(patient);
                }
            }
        }
    }
    
    
}