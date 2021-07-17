package com.teenthofabud.laundromat.manager.tax.model.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelException;
import com.teenthofabud.laundromat.manager.tax.model.converter.TaxModelDto2EntityConverter;
import com.teenthofabud.laundromat.manager.tax.model.converter.TaxModelEntity2VoConverter;
import com.teenthofabud.laundromat.manager.tax.model.converter.TaxModelForm2EntityConverter;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelDto;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelForm;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelMessageTemplate;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelVo;
import com.teenthofabud.laundromat.manager.tax.model.mapper.TaxModelEntitySelfMapper;
import com.teenthofabud.laundromat.manager.tax.model.mapper.TaxModelForm2EntityMapper;
import com.teenthofabud.laundromat.manager.tax.model.repository.TaxModelRepository;
import com.teenthofabud.laundromat.manager.tax.model.service.TaxModelService;
import com.teenthofabud.laundromat.manager.tax.model.validator.TaxModelDtoValidator;
import com.teenthofabud.laundromat.manager.tax.model.validator.TaxModelFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.tax.model.validator.TaxModelFormValidator;
import com.teenthofabud.laundromat.manager.tax.integration.type.validator.CurrencyTypeModelValidator;
import com.teenthofabud.laundromat.manager.tax.integration.type.validator.TaxTypeModelValidator;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;

@Component
@Slf4j
public class TaxModelServiceImpl implements TaxModelService {

    private static final Comparator<TaxModelVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private TOABBaseService toabBaseService;
    private ObjectMapper om;
    private TaxTypeModelValidator taxTypeModelValidator;
    private CurrencyTypeModelValidator currencyTypeModelValidator;

    private TaxModelEntity2VoConverter entity2VoConverter;
    private TaxModelForm2EntityConverter form2EntityConverter;
    private TaxModelDto2EntityConverter dto2EntityConverter;
    private TaxModelForm2EntityMapper form2EntityMapper;
    private TaxModelEntitySelfMapper entitySelfMapper;
    private TaxModelFormValidator formValidator;
    private TaxModelFormRelaxedValidator relaxedFormValidator;
    private TaxModelDtoValidator dtoValidator;
    private TaxModelRepository repository;

    @Autowired
    public void setToabBaseService(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setTaxTypeModelValidator(TaxTypeModelValidator taxTypeModelValidator) {
        this.taxTypeModelValidator = taxTypeModelValidator;
    }

    @Autowired
    public void setCurrencyTypeModelValidator(CurrencyTypeModelValidator currencyTypeModelValidator) {
        this.currencyTypeModelValidator = currencyTypeModelValidator;
    }

    @Autowired
    public void setRelaxedFormValidator(TaxModelFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setForm2EntityMapper(TaxModelForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(TaxModelEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(TaxModelDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setEntity2VoConverter(TaxModelEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(TaxModelDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityConverter(TaxModelForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(TaxModelRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(TaxModelFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<TaxModelVo> entity2DetailedVoList(List<TaxModelEntity> taxModelEntityList) {
        List<TaxModelVo> taxModelDetailsList = new ArrayList<>(taxModelEntityList.size());
        for(TaxModelEntity entity : taxModelEntityList) {
            TaxModelVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            taxModelDetailsList.add(vo);
        }
        return taxModelDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<TaxModelVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all TaxModelEntity by their natural ordering");
        List<TaxModelEntity> taxModelEntityList = repository.findAll();
        Set<TaxModelVo> naturallyOrderedSet = new TreeSet<TaxModelVo>(CMP_BY_NAME);
        for(TaxModelEntity entity : taxModelEntityList) {
            TaxModelVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} TaxModelVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public TaxModelVo retrieveDetailsById(Long id) throws TaxModelException {
        log.info("Requesting TaxModelEntity by id: {}", id);
        Optional<TaxModelEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No TaxModelEntity found by id: {}", id);
            throw new TaxModelException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        TaxModelEntity entity = optEntity.get();
        TaxModelVo vo = entity2VoConverter.convert(entity);
        log.info("Found TaxModelVo by id: {}", id);
        return vo;
    }

    @Override
    @Transactional(readOnly = true)
    public List<TaxModelVo> retrieveDetailsByTaxTypeModelId(Long taxTypeModelId) throws TaxModelException {
        log.info("Requesting TaxModelEntity that beLong to taxTypeModelId: {}", taxTypeModelId);

        log.debug("Validating taxTypeModelId: {}", taxTypeModelId);
        Errors internalErrors = new DirectFieldBindingResult(taxTypeModelId, "taxTypeModelId");
        taxTypeModelValidator.validate(taxTypeModelId, internalErrors);
        if(internalErrors.hasErrors()) {
            log.debug("taxTypeModelId is invalid");
            TaxErrorCode ec = TaxErrorCode.valueOf(internalErrors.getGlobalError().getCodes()[1]);
            log.debug("taxTypeModelId error detail: {}", ec);
            throw new TaxModelException(ec, new Object[] { "taxTypeModelId" });
        }
        log.debug("taxTypeModelId: {} is valid", taxTypeModelId);


        log.info("Requesting TaxModelEntity by taxTypeModelId: {}", taxTypeModelId);
        List<TaxModelEntity> taxModelEntities = repository.findByTaxTypeModelId(taxTypeModelId);
        if(taxModelEntities != null && !taxModelEntities.isEmpty()) {
            List<TaxModelVo> matchedTaxModelList = entity2DetailedVoList(taxModelEntities);
            log.info("Found {} TaxModelVo belonging to taxTypeModelId: {}", matchedTaxModelList.size(), taxTypeModelId);
            return matchedTaxModelList;
        }

        log.debug("No TaxModelVo found belonging to taxTypeModelId: {}", taxTypeModelId);
        throw new TaxModelException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "taxTypeModelId", String.valueOf(taxTypeModelId) });
    }

    @Override
    @Transactional(readOnly = true)
    public List<TaxModelVo> retrieveDetailsByCurrencyTypeModelId(Long currencyTypeModelId) throws TaxModelException {
        log.info("Requesting TaxModelEntity that beLong to currencyTypeModelId: {}", currencyTypeModelId);

        log.debug("Validating currencyTypeModelId: {}", currencyTypeModelId);
        Errors internalErrors = new DirectFieldBindingResult(currencyTypeModelId, "currencyTypeModelId");
        currencyTypeModelValidator.validate(currencyTypeModelId, internalErrors);
        if(internalErrors.hasErrors()) {
            log.debug("currencyTypeModelId is invalid");
            TaxErrorCode ec = TaxErrorCode.valueOf(internalErrors.getGlobalError().getCodes()[1]);
            log.debug("currencyTypeModelId error detail: {}", ec);
            throw new TaxModelException(ec, new Object[] { "currencyTypeModelId" });
        }
        log.debug("currencyTypeModelId: {} is valid", currencyTypeModelId);


        log.info("Requesting TaxModelEntity by currencyTypeModelId: {}", currencyTypeModelId);
        List<TaxModelEntity> taxModelEntities = repository.findByCurrencyTypeModelId(currencyTypeModelId);
        if(taxModelEntities != null && !taxModelEntities.isEmpty()) {
            List<TaxModelVo> matchedTaxModelList = entity2DetailedVoList(taxModelEntities);
            log.info("Found {} TaxModelVo belonging to currencyTypeModelId: {}", matchedTaxModelList.size(), currencyTypeModelId);
            return matchedTaxModelList;
        }

        log.debug("No TaxModelVo found belonging to currencyTypeModelId: {}", currencyTypeModelId);
        throw new TaxModelException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "currencyTypeModelId", String.valueOf(currencyTypeModelId) });
    }

    @Override
    @Transactional(readOnly = true)
    public List<TaxModelVo> retrieveAllMatchingDetailsByName(String name) throws TaxModelException {
        log.info("Requesting TaxModelEntity that match with name: {}", name);
        List<TaxModelEntity> taxModelEntityList = repository.findByNameContaining(name);
        if(taxModelEntityList != null && !taxModelEntityList.isEmpty()) {
            List<TaxModelVo> matchedTaxModelList = entity2DetailedVoList(taxModelEntityList);
            log.info("Found {} TaxModelVo matching with name: {}", matchedTaxModelList.size(),name);
            return matchedTaxModelList;
        }
        log.debug("No TaxModelVo found matching with name: {}", name);
        throw new TaxModelException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createTaxModel(TaxModelForm form) throws TaxModelException {
        log.info("Creating new TaxModelEntity");

        if(form == null) {
            log.debug("TaxModelForm provided is null");
            throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of TaxModelForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("TaxModelForm has {} errors", err.getErrorCount());
            TaxErrorCode ec = TaxErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TaxModelForm error detail: {}", ec);
            throw new TaxModelException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of TaxModelForm are valid");

        log.debug("Checking existence of TaxModelEntity with name: {} for taxTypeModelId: {} and currencyTypeModel.id: {}",
                form.getName(), form.getTaxTypeModelId(), form.getCurrencyTypeModel().getId());
        if(repository.existsByNameAndTaxTypeModelIdAndCurrencyTypeModelId(form.getName(), form.getTaxTypeModelId(), form.getCurrencyTypeModel().getId())) {
            log.debug("TaxModelEntity already exists with name: {} for taxTypeModelId: {} and currencyTypeModel.id: {}",
                    form.getName(), form.getTaxTypeModelId(), form.getCurrencyTypeModel().getId());
            throw new TaxModelException(TaxErrorCode.TAX_EXISTS,
                    new Object[]{ "name " + form.getName() + ", taxTypeModelId " + form.getTaxTypeModelId() + ", currencyTypeModel.id " + form.getCurrencyTypeModel().getId() });
        }
        log.debug("No TaxModelEntity exists with name: {} for taxTypeModelId: {} and currencyTypeModel.id: {}",
                form.getName(), form.getTaxTypeModelId(), form.getCurrencyTypeModel().getId());

        log.debug("Attempting to convert TaxModelForm to TaxModelEntity");
        TaxModelEntity expectedEntity = form2EntityConverter.convert(form);
        log.debug("Converted TaxModelForm to TaxModelEntity");

        log.debug("Saving {}", expectedEntity);
        TaxModelEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new TaxModelException(TaxErrorCode.TAX_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist TaxModelForm details" });
        }
        log.info("Created new TaxModelForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateTaxModel(Long id, TaxModelForm form) throws TaxModelException {
        log.info("Updating TaxModelForm by id: {}", id);

        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TAX_MODEL_ENTITY_ID.getValue(), id);
        Optional<TaxModelEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_NO_TAX_MODEL_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TaxModelException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_FOUND_TAX_MODEL_ENTITY_ID.getValue(), id);

        TaxModelEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TaxModelEntity is inactive with id: {}", id);
            throw new TaxModelException(TaxErrorCode.TAX_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TaxModelEntity is active with id: {}", id);

        if(form == null) {
            log.debug("TaxModelForm is null");
            throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TaxModelForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TaxModelForm has {} errors", err.getErrorCount());
            TaxErrorCode ec = TaxErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TaxModelForm error detail: {}", ec);
            throw new TaxModelException(ec, new Object[] { err.getFieldError().getField() });
        } else if (!allEmpty) {
            log.debug("All attributes of TaxModelForm are empty");
            throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TaxModelForm are valid");

        TaxModelEntity expectedEntity = null;

        try {
            Optional<TaxModelEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
            if(optExpectedEntity.isEmpty()) {
                log.debug("No new value for attributes of TaxModelForm");
                throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", "fields are expected with new values" });
            }
            log.debug("Successfully compared and copied attributes from TaxModelForm to TaxModelEntity");
            expectedEntity = optExpectedEntity.get();
        } catch (TOABBaseException e) {
            throw (TaxModelException) e;
        }

        checkUniquenessOfTaxModel(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from TaxModelEntity to TaxModelForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new TaxModelException(TaxErrorCode.TAX_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency tax model details" });
        }
        log.info("Updated existing TaxModelEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    public void deleteTaxModel(Long id) throws TaxModelException {
        log.info("Soft deleting TaxModelEntity by id: {}", id);

        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TAX_MODEL_ENTITY_ID.getValue(), id);
        Optional<TaxModelEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_NO_TAX_MODEL_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TaxModelException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_FOUND_TAX_MODEL_ENTITY_ID.getValue(), id);

        TaxModelEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TaxModelEntity is inactive with id: {}", id);
            throw new TaxModelException(TaxErrorCode.TAX_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TaxModelEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        TaxModelEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new TaxModelException(TaxErrorCode.TAX_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current tax model details with id:" + id });
        }

        log.info("Soft deleted existing TaxModelEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnTaxModel(Long id, List<PatchOperationForm> patches) throws TaxModelException {
        log.info("Patching TaxModelEntity by id: {}", id);

        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TAX_MODEL_ENTITY_ID.getValue(), id);
        Optional<TaxModelEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_NO_TAX_MODEL_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TaxModelException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_FOUND_TAX_MODEL_ENTITY_ID.getValue(), id);

        TaxModelEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("TaxModel patch list not provided");
            throw new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("TaxModel patch list has {} items", patches.size());


        log.debug("Validating patch list items for TaxModel");
        try {
            toabBaseService.validatePatches(patches, TaxErrorCode.TAX_EXISTS.getDomain() + ":Model");
            log.debug("All TaxModel patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the TaxModel patch item are invalid");
            throw new TaxModelException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for TaxModel");


        log.debug("Patching list items to TaxModelDto");
        TaxModelDto patchedTaxModelForm = new TaxModelDto();
        try {
            log.debug("Preparing patch list items for TaxModel");
            JsonNode taxModelDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch taxModelPatch = JsonPatch.fromJson(taxModelDtoTree);
            log.debug("Prepared patch list items for TaxModel");
            JsonNode blankTaxModelDtoTree = om.convertValue(new TaxModelDto(), JsonNode.class);
            JsonNode patchedTaxModelFormTree = taxModelPatch.apply(blankTaxModelDtoTree);
            log.debug("Applying patch list items to TaxModelDto");
            patchedTaxModelForm = om.treeToValue(patchedTaxModelFormTree, TaxModelDto.class);
            log.debug("Applied patch list items to TaxModelDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to TaxModelDto: {}", e);
            TaxModelException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in TaxModelDto");
                ex = new TaxModelException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new TaxModelException(TaxErrorCode.TAX_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to TypeModelDto: {}", e);
            throw new TaxModelException(TaxErrorCode.TAX_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TaxModelDto");

        log.debug("Validating patched TaxModelForm");
        Errors err = new DirectFieldBindingResult(patchedTaxModelForm, patchedTaxModelForm.getClass().getSimpleName());
        dtoValidator.validate(patchedTaxModelForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TaxModelForm has {} errors", err.getErrorCount());
            TaxErrorCode ec = TaxErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TaxModelForm error detail: {}", ec);
            throw new TaxModelException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched TaxModelForm are valid");

        checkUniquenessOfTaxModel(patchedTaxModelForm, actualEntity);

        log.debug("Comparatively copying patched attributes from TaxModelDto to TaxModelEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedTaxModelForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (TaxModelException) e;
        }
        log.debug("Comparatively copied patched attributes from TaxModelDto to TaxModelEntity");

        log.debug("Saving patched TaxModelEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched TaxModelEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete TaxModelEntity with id: {}", id);
            throw new TaxModelException(TaxErrorCode.TAX_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency tax model details with id:" + id });
        }
        log.info("Patched TaxModelEntity with id:{}", id);
    }

    private void checkUniquenessOfTaxModel(TaxModelDto patchedTaxModelForm, TaxModelEntity actualEntity) throws TaxModelException {
        // name = true, taxTypeModelId = true, currencyTypeModel.id = false
        if(patchedTaxModelForm.getName().isPresent() && patchedTaxModelForm.getTaxTypeModelId().isPresent()
                && (patchedTaxModelForm.getCurrencyTypeModel().isEmpty()
                || (patchedTaxModelForm.getCurrencyTypeModel().isPresent() && patchedTaxModelForm.getCurrencyTypeModel().get().getId().isEmpty()))) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID.getValue(),
                    patchedTaxModelForm.getName().get(), patchedTaxModelForm.getTaxTypeModelId().get());
            boolean sameEntitySw = patchedTaxModelForm.getName().get().equals(actualEntity.getName())
                    && patchedTaxModelForm.getTaxTypeModelId().get().equals(actualEntity.getTaxTypeModelId().toString());
            boolean duplicateEntitySw =  repository.existsByNameAndTaxTypeModelId(
                    patchedTaxModelForm.getName().get(), Long.parseLong(patchedTaxModelForm.getTaxTypeModelId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_TAX_TYPE_MODEL_ID.getValue(),
                        patchedTaxModelForm.getName().get(), patchedTaxModelForm.getTaxTypeModelId().get());
                throw new TaxModelException(TaxErrorCode.TAX_EXISTS,
                        new Object[]{ "name " + patchedTaxModelForm.getName().get() + ", taxTypeModelId " + patchedTaxModelForm.getTaxTypeModelId().get() });
            }
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID.getValue(),
                    patchedTaxModelForm.getName().get(), patchedTaxModelForm.getTaxTypeModelId().get());
        }

        // name = true, taxTypeModelId = true, currencyTypeModel.id = true
        if(patchedTaxModelForm.getName().isPresent() && patchedTaxModelForm.getTaxTypeModelId().isPresent()
                && patchedTaxModelForm.getCurrencyTypeModel().isPresent() && patchedTaxModelForm.getCurrencyTypeModel().get().getId().isPresent()) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    patchedTaxModelForm.getName().get(), patchedTaxModelForm.getTaxTypeModelId().get(),
                    patchedTaxModelForm.getCurrencyTypeModel().get().getId().get());
            boolean sameEntitySw = patchedTaxModelForm.getName().get().equals(actualEntity.getName())
                    && patchedTaxModelForm.getTaxTypeModelId().get().equals(actualEntity.getTaxTypeModelId().toString())
                    && patchedTaxModelForm.getCurrencyTypeModel().get().getId().get().equals(actualEntity.getCurrencyTypeModel().getId().toString());
            boolean duplicateEntitySw =  repository.existsByNameAndTaxTypeModelIdAndCurrencyTypeModelId(patchedTaxModelForm.getName().get(),
                    Long.parseLong(patchedTaxModelForm.getTaxTypeModelId().get()),
                    Long.parseLong(patchedTaxModelForm.getCurrencyTypeModel().get().getId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                        patchedTaxModelForm.getName().get(), patchedTaxModelForm.getTaxTypeModelId().get(),
                        patchedTaxModelForm.getCurrencyTypeModel().get().getId().get());
                throw new TaxModelException(TaxErrorCode.TAX_EXISTS,
                        new Object[]{ "name " + patchedTaxModelForm.getName().get() + ", taxTypeModelId " + patchedTaxModelForm.getTaxTypeModelId().get()
                                + ", currencyTypeModel.id " + patchedTaxModelForm.getCurrencyTypeModel().get().getId().get() });
            }
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    patchedTaxModelForm.getName().get(), patchedTaxModelForm.getTaxTypeModelId().get(),
                    patchedTaxModelForm.getCurrencyTypeModel().get().getId().get());
        }

        // name = true, taxTypeModelId = false, currencyTypeModel.id = true
        if(patchedTaxModelForm.getName().isPresent() && patchedTaxModelForm.getTaxTypeModelId().isEmpty()
                && patchedTaxModelForm.getCurrencyTypeModel().isPresent() && patchedTaxModelForm.getCurrencyTypeModel().get().getId().isPresent()) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    patchedTaxModelForm.getName().get(),  patchedTaxModelForm.getCurrencyTypeModel().get().getId().get());
            boolean sameEntitySw = patchedTaxModelForm.getName().get().equals(actualEntity.getName())
                    && patchedTaxModelForm.getCurrencyTypeModel().get().getId().get().equals(actualEntity.getCurrencyTypeModel().getId().toString());
            boolean duplicateEntitySw =  repository.existsByNameAndCurrencyTypeModelId(patchedTaxModelForm.getName().get(),
                    Long.parseLong(patchedTaxModelForm.getCurrencyTypeModel().get().getId().get()));
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                        patchedTaxModelForm.getName().get(), patchedTaxModelForm.getCurrencyTypeModel().get().getId().get());
                throw new TaxModelException(TaxErrorCode.TAX_EXISTS, new Object[]{ "name " + patchedTaxModelForm.getName().get()
                        + ", currencyTypeModel.id " + patchedTaxModelForm.getCurrencyTypeModel().get().getId().get() });
            }
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    patchedTaxModelForm.getName().get(),  patchedTaxModelForm.getCurrencyTypeModel().get().getId().get());
        }

        // name = true, taxTypeModelId = false, currencyTypeModel.id = false
        if(patchedTaxModelForm.getName().isPresent() && patchedTaxModelForm.getTaxTypeModelId().isEmpty()
                && (patchedTaxModelForm.getCurrencyTypeModel().isEmpty()
                || (patchedTaxModelForm.getCurrencyTypeModel().isPresent() && patchedTaxModelForm.getCurrencyTypeModel().get().getId().isEmpty()))) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    patchedTaxModelForm.getName().get(),  actualEntity.getTaxTypeModelId(), actualEntity.getCurrencyTypeModel().getId());
            boolean sameEntitySw = patchedTaxModelForm.getName().get().equals(actualEntity.getName());
            boolean duplicateEntitySw =  repository.existsByNameAndTaxTypeModelIdAndCurrencyTypeModelId(patchedTaxModelForm.getName().get(),
                    actualEntity.getTaxTypeModelId(), actualEntity.getCurrencyTypeModel().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                        patchedTaxModelForm.getName().get(), actualEntity.getTaxTypeModelId(), actualEntity.getCurrencyTypeModel().getId());
                throw new TaxModelException(TaxErrorCode.TAX_EXISTS, new Object[]{ "name " + patchedTaxModelForm.getName().get()
                        + ", taxTypeModelId " + actualEntity.getTaxTypeModelId() + ", currencyTypeModel.id " + actualEntity.getCurrencyTypeModel().getId() });
            }
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    patchedTaxModelForm.getName().get(), actualEntity.getTaxTypeModelId(), actualEntity.getCurrencyTypeModel().getId());
        }

    }

    private void checkUniquenessOfTaxModel(TaxModelForm taxModelForm, TaxModelEntity actualEntity) throws TaxModelException {
        // name = true, taxTypeModelId = true, currencyTypeModel.id = false
        if(StringUtils.hasText(StringUtils.trimWhitespace(taxModelForm.getName())) && taxModelForm.getTaxTypeModelId() != null
                && (taxModelForm.getCurrencyTypeModel() == null
                || (taxModelForm.getCurrencyTypeModel() != null && taxModelForm.getCurrencyTypeModel().getId() == null))) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID.getValue(),
                    taxModelForm.getName(), taxModelForm.getTaxTypeModelId());
            boolean sameEntitySw = taxModelForm.getName().equals(actualEntity.getName())
                    && taxModelForm.getTaxTypeModelId().equals(actualEntity.getTaxTypeModelId());
            boolean duplicateEntitySw =  repository.existsByNameAndTaxTypeModelId(
                    taxModelForm.getName(), taxModelForm.getTaxTypeModelId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_TAX_TYPE_MODEL_ID.getValue(),
                        taxModelForm.getName(), taxModelForm.getTaxTypeModelId());
                throw new TaxModelException(TaxErrorCode.TAX_EXISTS,
                        new Object[]{ "name " + taxModelForm.getName() + ", taxTypeModelId " + taxModelForm.getTaxTypeModelId() });
            }
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID.getValue(),
                    taxModelForm.getName(), taxModelForm.getTaxTypeModelId());
        }

        // name = true, taxTypeModelId = true, currencyTypeModel.id = true
        if(StringUtils.hasText(StringUtils.trimWhitespace(taxModelForm.getName())) && taxModelForm.getTaxTypeModelId() != null
                && taxModelForm.getCurrencyTypeModel() != null && taxModelForm.getCurrencyTypeModel().getId() != null) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    taxModelForm.getName(), taxModelForm.getTaxTypeModelId(),
                    taxModelForm.getCurrencyTypeModel().getId());
            boolean sameEntitySw = taxModelForm.getName().equals(actualEntity.getName())
                    && taxModelForm.getTaxTypeModelId().equals(actualEntity.getTaxTypeModelId().toString())
                    && taxModelForm.getCurrencyTypeModel().getId().equals(actualEntity.getCurrencyTypeModel().getId());
            boolean duplicateEntitySw =  repository.existsByNameAndTaxTypeModelIdAndCurrencyTypeModelId(taxModelForm.getName(),
                    taxModelForm.getTaxTypeModelId(), taxModelForm.getCurrencyTypeModel().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                        taxModelForm.getName(), taxModelForm.getTaxTypeModelId(),
                        taxModelForm.getCurrencyTypeModel().getId());
                throw new TaxModelException(TaxErrorCode.TAX_EXISTS,
                        new Object[]{ "name " + taxModelForm.getName() + ", taxTypeModelId " + taxModelForm.getTaxTypeModelId()
                                + ", currencyTypeModel.id " + taxModelForm.getCurrencyTypeModel().getId() });
            }
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    taxModelForm.getName(), taxModelForm.getTaxTypeModelId(),
                    taxModelForm.getCurrencyTypeModel().getId());
        }

        // name = true, taxTypeModelId = false, currencyTypeModel.id = true
        if(StringUtils.hasText(StringUtils.trimWhitespace(taxModelForm.getName())) && taxModelForm.getTaxTypeModelId() == null
                && taxModelForm.getCurrencyTypeModel() != null && taxModelForm.getCurrencyTypeModel().getId() != null) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    taxModelForm.getName(),  taxModelForm.getCurrencyTypeModel().getId());
            boolean sameEntitySw = taxModelForm.getName().equals(actualEntity.getName())
                    && taxModelForm.getCurrencyTypeModel().getId().equals(actualEntity.getCurrencyTypeModel().getId());
            boolean duplicateEntitySw =  repository.existsByNameAndCurrencyTypeModelId(taxModelForm.getName(), taxModelForm.getCurrencyTypeModel().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                        taxModelForm.getName(), taxModelForm.getCurrencyTypeModel().getId());
                throw new TaxModelException(TaxErrorCode.TAX_EXISTS, new Object[]{ "name " + taxModelForm.getName()
                        + ", currencyTypeModel.id " + taxModelForm.getCurrencyTypeModel().getId() });
            }
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    taxModelForm.getName(),  taxModelForm.getCurrencyTypeModel().getId());
        }

        // name = true, taxTypeModelId = false, currencyTypeModel.id = false
        if(StringUtils.hasText(StringUtils.trimWhitespace(taxModelForm.getName())) && taxModelForm.getTaxTypeModelId() == null
                && (taxModelForm.getCurrencyTypeModel() == null
                || (taxModelForm.getCurrencyTypeModel() != null && taxModelForm.getCurrencyTypeModel().getId() == null))) {
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    taxModelForm.getName(),  actualEntity.getTaxTypeModelId(), actualEntity.getCurrencyTypeModel().getId());
            boolean sameEntitySw = taxModelForm.getName().equals(actualEntity.getName());
            boolean duplicateEntitySw =  repository.existsByNameAndTaxTypeModelIdAndCurrencyTypeModelId(taxModelForm.getName(),
                    actualEntity.getTaxTypeModelId(), actualEntity.getCurrencyTypeModel().getId());
            if(sameEntitySw || duplicateEntitySw) {
                log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_EXISTS_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                        taxModelForm.getName(), actualEntity.getTaxTypeModelId(), actualEntity.getCurrencyTypeModel().getId());
                throw new TaxModelException(TaxErrorCode.TAX_EXISTS, new Object[]{ "name " + taxModelForm.getName()
                        + ", taxTypeModelId " + actualEntity.getTaxTypeModelId() + ", currencyTypeModel.id " + actualEntity.getCurrencyTypeModel().getId() });
            }
            log.debug(TaxModelMessageTemplate.MSG_TEMPLATE_TAX_MODEL_NON_EXISTENCE_BY_NAME_AND_TAX_TYPE_MODEL_ID_AND_CURRENCY_TYPE_MODEL_ID.getValue(),
                    taxModelForm.getName(), actualEntity.getTaxTypeModelId(), actualEntity.getCurrencyTypeModel().getId());
        }

    }

}