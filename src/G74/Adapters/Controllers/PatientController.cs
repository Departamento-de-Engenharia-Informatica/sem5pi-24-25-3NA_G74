using G74.Services;
using Microsoft.AspNetCore.Mvc;

namespace G74.Adapters.Controllers;

[Route("api/[controller]")]
[ApiController]
public class PatientController : ControllerBase
{
    private readonly IPatientAppService _patientAppService;

    public PatientController(IPatientAppService patientAppService)
    {
        _patientAppService = patientAppService;
    }

    [HttpGet("by-email/{email}")]
    public async Task<ActionResult<PatientDTO>> GetPatientByEmail(string email)
    {
        try
        {
            var patientDTO = await _patientAppService.GetPatientByEmail(email);
            return Ok(patientDTO);
        }
        catch (InvalidOperationException e)
        {
            return NotFound(e.Message);
        }
    }

    [HttpGet("{id}")]
    public async Task<ActionResult<PatientDTO>> GetPatientById(long id)
    {
        try
        {
            var patientDTO = await _patientAppService.GetPatientById(id);
            return Ok(patientDTO);
        }
        catch (InvalidOperationException e)
        {
            return NotFound(e.Message);
        }
    }

    [HttpPost]
    public async Task<ActionResult<PatientDTO>> RegisterPatient([FromBody] PatientDTO receivedPatientDto)
    {
        try
        {
            PatientDTO patientReturntDTO = await _patientAppService.RegisterPatient(receivedPatientDto);

            return CreatedAtAction(nameof(GetPatientByEmail), patientReturntDTO);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }

    [HttpPut("{id}")]
    public async Task<ActionResult<PatientDTO>> UpdatePatient(
            long id,
            [FromBody] PatientDTO updatedPatientDto
        )
    {
        if (updatedPatientDto == null)
        {
            return BadRequest("Invalid patient data.");
        }

        try
        {
            var patientDTO = await _patientAppService.UpdatePatient(id, updatedPatientDto);
            return Ok(patientDTO);
        }
        catch (InvalidOperationException e)
        {
            return NotFound(e.Message);
        }
        catch (Exception ex)
        {
            return BadRequest(ex.Message);
        }
    }
}