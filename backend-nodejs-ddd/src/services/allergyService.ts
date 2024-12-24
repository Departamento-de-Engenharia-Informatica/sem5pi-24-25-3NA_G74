import { Container, Service, Inject } from "typedi";
import config from "../../config";
import IAllergyService from "./IServices/IAllergyService";
import IAllergyRepo from "./IRepos/IAllergyRepo";
import {IAllergyDTO} from "../dto/IAllergyDTO";
import {Result} from "../core/logic/Result";
import {Allergy} from "../domain/Allergy";
import {AllergyMap} from "../mappers/AllergyMap";

@Service()
export default class AllergyService implements IAllergyService {
    constructor(
        @Inject(config.repos.allergy) private allergyRepo: IAllergyRepo,
    ) { }


    public async CreateAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>> {

        try {
            const allergyOrError = Allergy.create(allergyDTO);
            if (allergyOrError.isFailure) {
                return Result.fail<IAllergyDTO>(allergyOrError.errorValue());
            }
            const allergyResult = allergyOrError.getValue();

            await this.allergyRepo.save(allergyResult);

            const allergyDTOResult = AllergyMap.toDTO(allergyResult) as IAllergyDTO;
            return Result.ok<IAllergyDTO>(allergyDTOResult)
        } catch (e) {
            throw e;
        }
    }

    public async UpdateAllergy(allergyDTO: IAllergyDTO): Promise<Result<IAllergyDTO>> {
        try {
            const allergy = await this.allergyRepo.findById(allergyDTO.id);
            if (allergy === null) {
                return Result.fail<IAllergyDTO>("Allergy not found");
            }
            else {
                allergy.code = allergyDTO.code;
                allergy.designation = allergyDTO.designation;
                allergy.description = allergyDTO.description;
                await this.allergyRepo.save(allergy);
                const allergyDTOResult = AllergyMap.toDTO(allergy) as IAllergyDTO;
                return Result.ok<IAllergyDTO>(allergyDTOResult)
            }
        } catch (e) {
            throw e;
        }
    }

    public async SearchAllergy(code?: string): Promise<Result<IAllergyDTO[]>> {

        try {

            if (code != null) {
                const allergies = await this.allergyRepo.findByCode(code);
                const allergyDTO = AllergyMap.toDTO(allergies) as IAllergyDTO;

                let allergyDTOArray = new Array<IAllergyDTO>();
                allergyDTOArray.push(allergyDTO);
                return Result.ok<IAllergyDTO[]>(allergyDTOArray);
            }
            else {
                const allergies = await this.allergyRepo.findAll();
                const allergyDTO = allergies.map(allergy => AllergyMap.toDTO(allergy) as IAllergyDTO);
                return Result.ok<IAllergyDTO[]>(allergyDTO);
            }

        } catch (e) {
            throw e;
        }

    }


}