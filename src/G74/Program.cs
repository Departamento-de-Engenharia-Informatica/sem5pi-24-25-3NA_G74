using Microsoft.AspNetCore;
using Microsoft.EntityFrameworkCore;
using G74.Adapters.Repositories;
using G74.Domain.IRepositories;
using G74.Infrastructure.Persistence;
using G74.Services;
using G74.Mappers;

namespace G74
{
    public class Program
    {
        public static void Main(string[] args)
        {
            CreateWebHostBuilder(args).Build().Run();
        }

        public static IWebHostBuilder CreateWebHostBuilder(string[] args) =>
            WebHost.CreateDefaultBuilder(args)
                .UseStartup<Startup>();
    }
}
