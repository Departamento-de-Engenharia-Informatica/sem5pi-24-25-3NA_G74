using System.Text;
using G74.Adapters.Controllers;
using G74.Adapters.Repositories;
using G74.Domain.Aggregates.OperationType;
using G74.Domain.DomainServices;
using G74.Domain.IRepositories;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Infrastructure;
using G74.Infrastructure.Shared;
using G74.Mappers;
using G74.Services;
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
        serviceCollection.AddCors(options =>
        {
            options.AddPolicy("AllowAngularApp",
                builder => builder
                    .WithOrigins(
                        "http://localhost:4200", 
                        "https://localhost:4200",
                        "http://127.0.0.1:4200",
                        "https://127.0.0.1:4200",
						"http://vsgate-http.dei.isep.ipp.pt:10568/main/",
						"http://localhost:5000/main/"
						
                    )
                    .AllowAnyMethod()
                    .AllowAnyHeader()
                    .AllowCredentials());
        });
        
        
        serviceCollection.AddEndpointsApiExplorer().AddSwaggerGen().AddDbContext<BackofficeAppDbContext>(opt =>
            opt.UseSqlServer(Configuration.GetConnectionString("DefaultConnection"))
                .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>()
                .UseLoggerFactory(LoggerFactory.Create(builder => builder.AddConsole())));
    
        ConfigureMyServices(serviceCollection);
        
        ConfigureAuthentication(serviceCollection);

        ConfigureAuthorization(serviceCollection);
        /*
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
        */

        serviceCollection.AddControllers();
    }

    public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
    {
        if (env.IsDevelopment())
        {
            app.UseCors("AllowAngularApp");
            app.UseSwagger();
            app.UseSwaggerUI();
            app.UseDeveloperExceptionPage();
        }
        else
        {
            app.UseCors("AllowAngularApp");
            // The default HSTS value is 30 days. You may want to change this for production scenarios, see https://aka.ms/aspnetcore-hsts.
            app.UseHsts();
        }

        
        
        app.UseHttpsRedirection();
        app.UseRouting();
        //app.UseSession();
        app.UseAuthentication();
        app.UseAuthorization();


        app.UseEndpoints(endpoints => { endpoints.MapControllers(); });
    }


    public void ConfigureMyServices(IServiceCollection services)
    {
        services.AddTransient<IUnitOfWork,UnitOfWork>();
        services.AddTransient<UserToDtoMapper>();
        services.AddScoped<UserController>();
        services.AddScoped<IRepoUser, RepoUser>();
        services.AddScoped<IPatientAppService, PatientAppService>();
        services.AddScoped<IPatientRepository, PatientRepository>();
        services.AddTransient<PatientDataModelMapper>();
        services.AddTransient<PatientMapper>();
        services.AddScoped<IMedicalRecordNumberGenerator, MedicalRecordNumberGenerator>();
        services.AddHostedService<PatientDeletionService>();
        services.AddScoped<UserAppService>();
        services.AddTransient<UserToDataModelMapper>();
        services.AddScoped<IAppServiceOperationRequest, AppServiceOperationRequest>();
        services.AddScoped<AuthController>();
        services.AddTransient<IOperationRequestRepository, OperationRequestRepository>();
        services.AddScoped<GmailEmailService>();
        services.AddScoped<IStaffRepository, StaffRepository>();
        services.AddScoped<IStaffService, StaffService>();
        services.AddScoped<StaffController>();
        services.AddScoped<ISpecializationRepository, SpecializationRepository>();
        services.AddScoped<ISpecializationService, SpecializationService>();
        services.AddScoped<SpecializationController>();
        services.AddScoped<IOperationTypeRepository,OperationTypeRepository>();
        services.AddScoped<IAppointmentRepository,AppointmentRepository>();
        services.AddScoped<IAppServiceAppointment,AppServiceAppointment>();
        services.AddScoped<ISurgeryRoomRepository,SurgeryRoomRepository>();
        services.AddTransient<OperationTypeToDataModelMapper>();
        services.AddTransient<AppointmentToDataModelMapper>();
        services.AddTransient<SurgeryRoomToDataModelMapper>();
        services.AddScoped<OptimizationModuleService>();
        services.AddScoped<OptimizationModuleController>();
        services.AddScoped<OperationTypeService>();
        services.AddScoped<OperationTypeController>();
        services.AddTransient<OperationTypeDtoMapper>();


        // For authentication and authorization purposes
        services.AddScoped<ITokenService, TokenService>();
        services.AddScoped<IProviderIAMService, GoogleIAMService>();
        services.AddScoped<IAuthService, AuthService>();
        services.AddHttpContextAccessor();
        


    }

    private void ConfigureAuthentication(IServiceCollection serviceCollection)
    {
        serviceCollection.AddAuthentication(JwtBearerDefaults.AuthenticationScheme)
            .AddJwtBearer(options =>
            {
                options.TokenValidationParameters = new TokenValidationParameters
                {
                    ValidateIssuerSigningKey = true,
                    IssuerSigningKey =
                        new SymmetricSecurityKey(Encoding.UTF8.GetBytes(Configuration["JwtSettings:SecretKey"])),
                    ValidateIssuer = true,
                    ValidIssuer = Configuration["JwtSettings:Issuer"],
                    ValidateAudience = true,
                    ValidAudience = Configuration["JwtSettings:Audience"],
                    ValidateLifetime = true,
                    ClockSkew = TimeSpan.Zero // Remover atraso no tempo do token
                };
            });
    }

    private void ConfigureAuthorization(IServiceCollection serviceCollection)
    {
        serviceCollection.AddAuthorization(options =>
        {
            options.AddPolicy("Admin", policy => policy.RequireRole(Role.Admin.ToString()));
            options.AddPolicy("Patient", policy => policy.RequireRole(Role.Patient.ToString()));
            options.AddPolicy("Nurse", policy => policy.RequireRole(Role.Nurse.ToString()));
            options.AddPolicy("Technician", policy => policy.RequireRole(Role.Technician.ToString()));
            options.AddPolicy("Doctor", policy => policy.RequireRole(Role.Doctor.ToString()));
        });
    }
}