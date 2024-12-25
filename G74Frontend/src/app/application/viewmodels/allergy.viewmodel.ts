
import { Injectable } from "@angular/core";
import { Observable } from "rxjs";

@Injectable({
  providedIn: 'root'
})


export class AllergyViewModel {

  constructor(private allergyService: AllergyService) { }

  createAllergy(allergy: AllergyDTO): Observable<AllergyDTO> {
    return this.allergyService.createAllergy(allergy);
  }

  updateAllergy(dto: AllergyDTO): Observable<AllergyDTO> {
    return this.allergyService.updateAllergy(dto);
  }


  searchAllergy(code?: string): Observable<AllergyDTO[]> {
    return this.allergyService.searchAllergy(code);
  }

}
