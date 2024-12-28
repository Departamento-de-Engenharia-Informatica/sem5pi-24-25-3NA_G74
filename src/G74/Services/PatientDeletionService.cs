using System.Collections;
using G74.DataModel;
using G74.Domain;
using G74.Domain.IRepositories;
using Org.BouncyCastle.Utilities;

namespace G74.Services;

public class PatientDeletionService : BackgroundService
{
    private readonly IServiceProvider _serviceProvider;
    private readonly IConfiguration _configuration;

    public PatientDeletionService(IServiceProvider serviceProvider, IConfiguration configuration)
    {
        _serviceProvider = serviceProvider;
        _configuration = configuration;
    }

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        
        string timeStr = _configuration["GPRD:RefreshDelete"] ?? "1";

        int time = int.Parse(timeStr);
        
        while (!stoppingToken.IsCancellationRequested)
        {
            await Task.Delay(TimeSpan.FromMinutes(time), stoppingToken);
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