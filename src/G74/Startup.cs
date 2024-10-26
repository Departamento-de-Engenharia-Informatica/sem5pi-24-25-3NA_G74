using System.Text;
using System.Text.Json;
using DefaultNamespace;
using G74.Adapters.Controllers;
using G74.Adapters.Repositories;
using G74.Domain.DomainServices;
using G74.Domain.Factory;
using G74.Domain.IRepositories;
using G74.DTO;
using G74.Infrastructure.Shared;
using G74.Mappers;
using G74.Services;
using Microsoft.AspNetCore.Authentication.Cookies;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using Microsoft.IdentityModel.Tokens;

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

        serviceCollection.AddDistributedMemoryCache();

        serviceCollection.AddSession(options =>
        {
            options.IdleTimeout = TimeSpan.FromMinutes(30);
            options.Cookie.HttpOnly = true;
            options.Cookie.IsEssential = true;
            options.Cookie.SameSite = SameSiteMode.Strict;
            options.Cookie.SecurePolicy = CookieSecurePolicy.Always;
        });

        serviceCollection.AddAuthentication(CookieAuthenticationDefaults.AuthenticationScheme)
            .AddCookie(options =>
            {
                options.LoginPath = "/api/auth/login";
                options.LogoutPath = "/api/auth/logout";
                options.AccessDeniedPath = "/api/auth/access-denied";
                options.ExpireTimeSpan = TimeSpan.FromMinutes(30); 
                options.SlidingExpiration = true; 
                options.Events.OnSigningOut = context =>
                {
                    context.HttpContext.Response.Cookies.Delete(".AspNetCore.Cookies");
                    return Task.CompletedTask;
                };
            });

        serviceCollection.AddAuthorization(options =>
        {
            options.AddPolicy("RequireAdministratorRole", policy => policy.RequireRole("Admin"));
            options.AddPolicy("RequireDoctorRole", policy => policy.RequireRole("Doctor"));
            options.AddPolicy("RequireNurseRole", policy => policy.RequireRole("Nurse"));
            options.AddPolicy("RequireTechnicianRole", policy => policy.RequireRole("Technician"));
            options.AddPolicy("RequirePatientRole", policy => policy.RequireRole("Patient"));
        });

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
        app.UseSession();
        app.UseAuthentication();
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
        services.AddScoped<IAppServiceOperationRequest, AppServiceOperationRequest>();
        services.AddScoped<AuthController>();
        //services.AddTransient<IOperationRequestRepository, OperationRequestRepository>();
        services.AddScoped<GmailEmailService>();
        // Add Staff-related services
        services.AddScoped<IStaffRepository, StaffRepository>();
        services.AddScoped<StaffAppService>();
        services.AddScoped<StaffFactory>();
        services.AddScoped<StaffMapper>();
        services.AddScoped<StaffController>();
        services.AddTransient<StaffToDto>();

    }
}