package com.teenthofabud.laundromat.manager.tax.lov.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.laundromat.manager.tax.error.TaxErrorCode;
import com.teenthofabud.laundromat.manager.tax.lov.converter.TaxLOVDto2EntityConverter;
import com.teenthofabud.laundromat.manager.tax.lov.converter.TaxLOVEntity2VoConverter;
import com.teenthofabud.laundromat.manager.tax.lov.converter.TaxLOVForm2EntityConverter;
import com.teenthofabud.laundromat.manager.tax.lov.data.*;
import com.teenthofabud.laundromat.manager.tax.lov.mapper.TaxLOVEntitySelfMapper;
import com.teenthofabud.laundromat.manager.tax.lov.mapper.TaxLOVForm2EntityMapper;
import com.teenthofabud.laundromat.manager.tax.lov.repository.TaxLOVRepository;
import com.teenthofabud.laundromat.manager.tax.lov.service.TaxLOVService;
import com.teenthofabud.laundromat.manager.tax.lov.validator.TaxLOVDtoValidator;
import com.teenthofabud.laundromat.manager.tax.lov.validator.TaxLOVFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.tax.lov.validator.TaxLOVFormValidator;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelEntity;
import com.teenthofabud.laundromat.manager.tax.model.data.TaxModelException;
import com.teenthofabud.laundromat.manager.tax.model.service.TaxModelService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.validation.DirectFieldBindingResult;
import org.springframework.validation.Errors;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

@Component
@Slf4j
public class TaxLOVServiceImpl implements TaxLOVService {

    private static final Comparator<TaxLOVVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private TaxLOVEntity2VoConverter entity2VoConverter;
    private TaxLOVForm2EntityConverter form2EntityConverter;
    private TaxLOVDto2EntityConverter dto2EntityConverter;
    private TaxLOVForm2EntityMapper form2EntityMapper;
    private TaxLOVEntitySelfMapper entitySelfMapper;
    private TaxLOVFormValidator formValidator;
    private TaxLOVFormRelaxedValidator relaxedFormValidator;
    private TaxLOVDtoValidator dtoValidator;
    private TaxLOVRepository repository;
    private TOABBaseService toabBaseService;
    private TaxModelService taxModelService;
    private ObjectMapper om;

    @Autowired
    public void setTaxModelService(TaxModelService taxModelService) {
        this.taxModelService = taxModelService;
    }

    @Autowired
    public void setEntity2VoConverter(TaxLOVEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(TaxLOVDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(TaxLOVForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(TaxLOVEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(TaxLOVFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchOperationValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(TaxLOVDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(TaxLOVForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(TaxLOVRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(TaxLOVFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<TaxLOVVo> entity2DetailedVoList(List<TaxLOVEntity> taxLovEntityList) {
        List<TaxLOVVo> taxLovDetailsList = new ArrayList<>(taxLovEntityList.size());
        for(TaxLOVEntity entity : taxLovEntityList) {
            TaxLOVVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            taxLovDetailsList.add(vo);
        }
        return taxLovDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<TaxLOVVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all TaxLOVEntity by their natural ordering");
        List<TaxLOVEntity> taxLovEntityList = repository.findAll();
        Set<TaxLOVVo> naturallyOrderedSet = new TreeSet<TaxLOVVo>(CMP_BY_NAME);
        for(TaxLOVEntity entity : taxLovEntityList) {
            TaxLOVVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} TaxLOVVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public TaxLOVVo retrieveDetailsById(Long id) throws TaxLOVException {
        log.info("Requesting TaxLOVEntity by id: {}", id);
        Optional<TaxLOVEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No TaxLOVEntity found by id: {}", id);
            throw new TaxLOVException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        TaxLOVEntity entity = optEntity.get();
        TaxLOVVo vo = entity2VoConverter.convert(entity);
        log.info("Found TaxLOVVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<TaxLOVVo> retrieveAllMatchingDetailsByName(String name) throws TaxLOVException {
        log.info("Requesting TaxLOVEntity that match with name: {}", name);
        List<TaxLOVEntity> taxLovEntityList = repository.findByNameContaining(name);
        if(taxLovEntityList != null && !taxLovEntityList.isEmpty()) {
            List<TaxLOVVo> matchedTaxLOVList = entity2DetailedVoList(taxLovEntityList);
            log.info("Found {} TaxLOVVo matching with name: {}", matchedTaxLOVList.size(),name);
            return matchedTaxLOVList;
        }
        log.debug("No TaxLOVVo found matching with name: {}", name);
        throw new TaxLOVException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createTaxLOV(TaxLOVForm form) throws TaxLOVException {
        log.info("Creating new TaxLOVEntity");

        if(form == null) {
            log.debug("TaxLOVForm provided is null");
            throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of TaxLOVForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("TaxLOVForm has {} errors", err.getErrorCount());
            TaxErrorCode ec = TaxErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TaxLOVForm error detail: {}", ec);
            throw new TaxLOVException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of TaxLOVForm are valid");

        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_EXISTENCE_BY_NAME.getValue(), form.getName());
        TaxLOVEntity expectedEntity = form2EntityConverter.convert(form);
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new TaxLOVException(TaxErrorCode.TAX_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        TaxLOVEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new TaxLOVException(TaxErrorCode.TAX_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist TaxLOVForm details" });
        }
        log.info("Created new TaxLOVForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateTaxLOV(Long id, TaxLOVForm form) throws TaxLOVException {
        log.info("Updating TaxLOVForm by id: {}", id);

        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TAX_LOV_ENTITY_ID.getValue(), id);
        Optional<TaxLOVEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_NO_TAX_LOV_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TaxLOVException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_FOUND_TAX_LOV_ENTITY_ID.getValue(), id);

        TaxLOVEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TaxLOVEntity is inactive with id: {}", id);
            throw new TaxLOVException(TaxErrorCode.TAX_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TaxLOVEntity is active with id: {}", id);

        if(form == null) {
            log.debug("TaxLOVForm is null");
            throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of TaxLOVForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("TaxLOVForm has {} errors", err.getErrorCount());
            TaxErrorCode ec = TaxErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("TaxLOVForm error detail: {}", ec);
            throw new TaxLOVException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of TaxLOVForm are empty");
            throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of TaxLOVForm are valid");

        Optional<TaxLOVEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of TaxLOVForm");
            throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from TaxLOVForm to TaxLOVEntity");

        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_EXISTENCE_BY_NAME.getValue(), form.getName());
        TaxLOVEntity expectedEntity = optExpectedEntity.get();
        if(actualEntity.getName().compareTo(expectedEntity.getName()) == 0 || repository.existsByName(expectedEntity.getName())) {
            log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new TaxLOVException(TaxErrorCode.TAX_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from TaxLOVEntity to TaxLOVForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new TaxLOVException(TaxErrorCode.TAX_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency tax LOV details" });
        }
        log.info("Updated existing TaxLOVEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    //@Transactional(rollbackFor = TaxModelException.class, propagation = Propagation.REQUIRES_NEW, isolation = Isolation.READ_COMMITTED)
    public void deleteTaxLOV(Long id) throws TaxLOVException {
        log.info("Soft deleting TaxLOVEntity by id: {}", id);

        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TAX_LOV_ENTITY_ID.getValue(), id);
        Optional<TaxLOVEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_NO_TAX_LOV_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TaxLOVException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_FOUND_TAX_LOV_ENTITY_ID.getValue(), id);

        TaxLOVEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("TaxLOVEntity is inactive with id: {}", id);
            throw new TaxLOVException(TaxErrorCode.TAX_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("TaxLOVEntity is active with id: {}", id);

        Set<TaxModelEntity> modelEntities = actualEntity.getModelEntities();
        if(modelEntities != null) {
            log.error("Trying to soft delete {} associated TaxModelEntity with Tax LOV id: {}", modelEntities.size(), id);
            for(TaxModelEntity taxModelEntity : modelEntities) {
                Long taxModelId = taxModelEntity.getId();
                try {
                    taxModelService.deleteTaxModel(taxModelId);
                } catch (TaxModelException e) {
                    log.debug("Unable to soft delete TaxModelEntity with id: {}", taxModelId);
                    log.error("Soft deletion failed for TaxModelEntity with Tax LOV id: {} because: {}", id, e);
                    // throwing unchecked exception to trigger transaction rollback
                    throw new TOABSystemException(TaxErrorCode.TAX_ACTION_FAILURE, new Object[] { "deletion", "all associated Tax Models"});
                }
            }
            log.error("Successfully soft deleted {} associated TaxModelEntity with Tax LOV id: {}", modelEntities.size(), id);
        } else {
            log.error("No TaxModelEntity associated with Tax LOV id: {}", id);
        }

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        TaxLOVEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new TaxLOVException(TaxErrorCode.TAX_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current tax LOV details with id:" + id });
        }

        log.info("Soft deleted existing TaxLOVEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnTaxLOV(Long id, List<PatchOperationForm> patches) throws TaxLOVException {
        log.info("Patching TaxLOVEntity by id: {}", id);

        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_TAX_LOV_ENTITY_ID.getValue(), id);
        Optional<TaxLOVEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_NO_TAX_LOV_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new TaxLOVException(TaxErrorCode.TAX_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_FOUND_TAX_LOV_ENTITY_ID.getValue(), id);

        TaxLOVEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("TaxLOV patch list not provided");
            throw new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("TaxLOV patch list has {} items", patches.size());


        log.debug("Validating patch list items for TaxLOV");
        try {
            toabBaseService.validatePatches(patches, TaxErrorCode.TAX_EXISTS.getDomain() + ":LOV");
            log.debug("All TaxLOV patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the TaxLOV patch item are invalid");
            throw new TaxLOVException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for TaxLOV");


        log.debug("Patching list items to TaxLOVDto");
        TaxLOVDto patchedTaxLOVForm = new TaxLOVDto();
        try {
            log.debug("Preparing patch list items for TaxLOV");
            JsonNode taxLovDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch taxLovPatch = JsonPatch.fromJson(taxLovDtoTree);
            log.debug("Prepared patch list items for TaxLOV");
            JsonNode blankTaxLOVDtoTree = om.convertValue(new TaxLOVDto(), JsonNode.class);
            JsonNode patchedTaxLOVFormTree = taxLovPatch.apply(blankTaxLOVDtoTree);
            log.debug("Applying patch list items to TaxLOVDto");
            patchedTaxLOVForm = om.treeToValue(patchedTaxLOVFormTree, TaxLOVDto.class);
            log.debug("Applied patch list items to TaxLOVDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to TaxLOVDto: {}", e);
            TaxLOVException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in TaxLOVDto");
                ex = new TaxLOVException(TaxErrorCode.TAX_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new TaxLOVException(TaxErrorCode.TAX_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to TaxLOVDto: {}", e);
            throw new TaxLOVException(TaxErrorCode.TAX_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to TaxLOVDto");

        log.debug("Validating patched TaxLOVDto");
        Errors err = new DirectFieldBindingResult(patchedTaxLOVForm, patchedTaxLOVForm.getClass().getSimpleName());
        dtoValidator.validate(patchedTaxLOVForm, err);
        if(err.hasErrors()) {
            log.debug("Patched TaxLOVDto has {} errors", err.getErrorCount());
            TaxErrorCode ec = TaxErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched TaxLOVDto error detail: {}", ec);
            throw new TaxLOVException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched TaxLOVDto are valid");

        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_EXISTENCE_BY_NAME.getValue(), patchedTaxLOVForm.getName().get());
        if(actualEntity.getName().compareTo(patchedTaxLOVForm.getName().get()) == 0 || repository.existsByName(patchedTaxLOVForm.getName().get())) {
            log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_EXISTS_BY_NAME.getValue(), patchedTaxLOVForm.getName().get());
            throw new TaxLOVException(TaxErrorCode.TAX_EXISTS,
                    new Object[]{ "name: " + patchedTaxLOVForm.getName().get() });
        }
        log.debug(TaxLOVMessageTemplate.MSG_TEMPLATE_TAX_LOV_NON_EXISTENCE_BY_NAME.getValue(), patchedTaxLOVForm.getName().get());


        log.debug("Comparatively copying patched attributes from TaxLOVDto to TaxLOVEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedTaxLOVForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (TaxLOVException) e;
        }
        log.debug("Comparatively copied patched attributes from TaxLOVDto to TaxLOVEntity");

        log.debug("Saving patched TaxLOVEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched TaxLOVEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete TaxLOVEntity with id:{}", id);
            throw new TaxLOVException(TaxErrorCode.TAX_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency tax LOV details with id:" + id });
        }
        log.info("Patched TaxLOVEntity with id:{}", id);
    }
}