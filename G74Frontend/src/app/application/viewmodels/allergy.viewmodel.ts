
import { Injectable } from "@angular/core";
import { Observable } from "rxjs";
import {AllergyService} from "../../domain/services/allergy.service";
import {AllergyDTO} from "../../dto/allergy.dto";

@Injectable({
  providedIn: 'root'
})


export class AllergyViewModel {

  constructor(private allergyService: AllergyService) { }

  createAllergy(allergy: AllergyDTO): Observable<AllergyDTO> {
    return this.allergyService.createAllergy(allergy);
  }

  updateAllergy(dto: AllergyDTO): Observable<AllergyDTO> {

    const code = dto.code;
    const updatedData = {
      designation: dto.designation,
      description: dto.description
    }

    return this.allergyService.updateAllergy(code, updatedData);
  }


  searchAllergy(code?: string, designation?: string): Observable<AllergyDTO[]> {
    return this.allergyService.searchAllergy(code, designation);
  }

}
