using System.Text.Json;
using G74.Adapters.Controllers;
using G74.Adapters.Repositories;
using G74.Domain.DomainServices;
using G74.Domain.IRepositories;
using G74.DTO;
using G74.Infrastructure.Shared;
using G74.Mappers;
using G74.Services;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;

namespace G74;

public class Startup
{
    public IConfiguration Configuration { get; }

    public Startup(IConfiguration configuration)
    {
        Configuration = configuration;
    }


    public void ConfigureServices(IServiceCollection serviceCollection)
    {
        serviceCollection.AddEndpointsApiExplorer().AddSwaggerGen().AddDbContext<BackofficeAppDbContext>(opt =>
            opt.UseSqlServer(Configuration.GetConnectionString("DefaultConnection"))
                .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>()
                .UseLoggerFactory(LoggerFactory.Create(builder => builder.AddConsole())));
        
        ConfigureMyServices(serviceCollection);

        serviceCollection.AddControllers();
    }

    public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
    {
        if (env.IsDevelopment())
        {

            app.UseSwagger();
            app.UseSwaggerUI();
            
            app.UseDeveloperExceptionPage();
        }
        else
        {
            // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
            app.UseHsts();
        }

        app.UseHttpsRedirection();

        app.UseRouting();

        app.UseAuthorization();

        app.UseEndpoints(endpoints => { endpoints.MapControllers(); });
    }


    public void ConfigureMyServices(IServiceCollection services)
    {

        services.AddTransient<UserToDTO>();
        services.AddScoped<UserController>();
        services.AddScoped<IRepoUser, RepoUser>();
        services.AddScoped<IPatientAppService, PatientAppService>();
        services.AddScoped<IPatientRepository, PatientRepository>();
        services.AddScoped<IMedicalRecordNumberGenerator, MedicalRecordNumberGenerator>();
        services.AddScoped<UserAppService>();
        services.AddTransient<UserMapper>();
        services.AddTransient<UserToDataMapper>();
    }
}