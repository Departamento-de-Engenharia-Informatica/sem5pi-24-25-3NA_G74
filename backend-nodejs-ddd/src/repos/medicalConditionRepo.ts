import { Service, Inject } from 'typedi';

import { Document, FilterQuery, Model } from 'mongoose';

import IMedicalConditionRepo from '../services/IRepos/IMedicalConditionRepo';

import { MedicalCondition } from "../domain/medicalCondition";

import { MedicalConditionId } from '../domain/medicalConditionId';

import { MedicalConditionMap } from '../mappers/MedicalConditionMap';
import { IMedicalConditionPersistence } from '../dataschema/IMedicalConditionPersistence';


@Service()
export default class MedicalConditionRepo implements IMedicalConditionRepo {
    private models: any;

    constructor(
        @Inject('medicalConditionSchema') private medicalConditionSchema: Model<any & Document>,
        @Inject('logger') private logger
    ) { }


    public async exists(medicalConditionId: MedicalConditionId): Promise<boolean> {

        const idX = medicalConditionId instanceof MedicalConditionId ? (<MedicalConditionId>medicalConditionId).id.toValue() : medicalConditionId;

        const query = { domainId: idX };
        const medicalConditionDocument = await this.medicalConditionSchema.findOne(query);

        return !!medicalConditionDocument;

    }

    public async save(medicalCondition: MedicalCondition): Promise<MedicalCondition> {

        const query = { domainId: medicalCondition.id.toValue() };

        const medicalConditionDocument = await this.medicalConditionSchema.findOne(query);

        try {
            if (medicalConditionDocument === null) {
                const rawMedicalCondition: any = MedicalConditionMap.toPersistence(medicalCondition);

                const medicalConditionCreated = await this.medicalConditionSchema.create(rawMedicalCondition);

                return MedicalConditionMap.toDomain(medicalConditionCreated);
            } else {

                medicalConditionDocument.designation = medicalCondition.designation;
                medicalConditionDocument.description = medicalCondition.description;

                await medicalConditionDocument.save();

                return medicalCondition;
            }
        } catch (err) {
            throw err;
        }
    }

    public async update(medicalCondition: MedicalCondition): Promise<MedicalCondition> {

        const query = { domainId: medicalCondition.id.toString() };
        const medicalConditionDocument = await this.medicalConditionSchema.findOne(query);

        if (medicalConditionDocument === null) {
            throw Error("medical condition not found, couldn't update");
        } else {
            medicalConditionDocument.description = medicalCondition.description;
            await medicalConditionDocument.save();

            return medicalCondition;
        }


    }

    public async findByDescription(description: string): Promise<MedicalCondition> {

        const query = { description: description.toString() };
        const medicalConditionRecord = await this.medicalConditionSchema.findOne(query)

        if (medicalConditionRecord != null) {
            return MedicalConditionMap.toDomain(medicalConditionRecord);
        }
        else {
            throw Error("medical condition not found");
        }
    }

    public async findByDesignation(designation: string): Promise<MedicalCondition[]> {
    
        const query = { designation: designation.toString() };
        const medicalConditionRecords = await this.medicalConditionSchema.find(query);

        if (medicalConditionRecords != null) {
            return medicalConditionRecords.map((medicalConditionRecord) => MedicalConditionMap.toDomain(medicalConditionRecord));
        } else {
            throw Error("medical condition not found");
        }
    }


    public async findAll(): Promise<MedicalCondition[]> {

        const medicalConditionRecord = await this.medicalConditionSchema.find();

        if (medicalConditionRecord != null) {
            return medicalConditionRecord.map((medicalConditionRecord) => MedicalConditionMap.toDomain(medicalConditionRecord));
        } else {
            throw Error("medical condition not found");
        }

    }
    public async findById(medicalConditionId: MedicalConditionId | string): Promise<MedicalCondition> {

        const idX = medicalConditionId instanceof MedicalConditionId ? (<MedicalConditionId>medicalConditionId).id.toValue() : medicalConditionId;

        const query = { domainId: idX };
        const medicalConditionDocument = await this.medicalConditionSchema.findOne(query as FilterQuery<IMedicalConditionPersistence & Document>);

        if (medicalConditionDocument != null) {
            return MedicalConditionMap.toDomain(medicalConditionDocument);
        }
        else {
            throw Error("medical condition not found");
        }
    }

    public async findByMedicalConditionCode(medicalConditionCode: string): Promise<MedicalCondition> {
        
        const query = { medicalConditionCode: medicalConditionCode.toString() };
        const medicalConditionRecord = await this.medicalConditionSchema.findOne(query)

        if (medicalConditionRecord != null) {
            return MedicalConditionMap.toDomain(medicalConditionRecord);
        }
        else {
            throw Error("medical condition not found");
        }
    }

}