using System.Collections;
using G74.DataModel;
using G74.Domain;
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
            var patients = (await patientRepository.GetPatientsReadyForDeletion()).ToList();

            if (patients.Any())
            {
                foreach (var patient in patients)
                {
                    await patientRepository.DeletePatientDefinitive(patient);
                }
            }
        }
    }
}