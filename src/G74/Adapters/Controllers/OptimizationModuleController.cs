using G74.Services;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;

[ApiController]
[Route("api/[controller]")]
public class OptimizationModuleController : ControllerBase
{
    private readonly OptimizationModuleService _optimizationModuleService;

    public OptimizationModuleController(OptimizationModuleService optimizationModuleService)
    {
        _optimizationModuleService = optimizationModuleService;
    }
    
    [HttpGet("export-data")]
    public async Task<IActionResult> ExportData()
    {
        try
        {
            await _optimizationModuleService.ExportAllDataToProlog();
            return Ok("Staff data successfully exported to Prolog file.");
        }
        catch (Exception ex)
        {
            return StatusCode(500, $"Error exporting data: {ex.Message}");
        }
    }
}