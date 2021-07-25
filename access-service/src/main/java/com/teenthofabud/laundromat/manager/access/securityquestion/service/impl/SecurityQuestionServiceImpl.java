package com.teenthofabud.laundromat.manager.access.securityquestion.service.impl;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.fge.jsonpatch.JsonPatch;
import com.github.fge.jsonpatch.JsonPatchException;
import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.error.TOABBaseException;
import com.teenthofabud.core.common.error.TOABSystemException;
import com.teenthofabud.core.common.service.TOABBaseService;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.securityquestion.converter.SecurityQuestionDto2EntityConverter;
import com.teenthofabud.laundromat.manager.access.securityquestion.converter.SecurityQuestionEntity2VoConverter;
import com.teenthofabud.laundromat.manager.access.securityquestion.converter.SecurityQuestionForm2EntityConverter;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.*;
import com.teenthofabud.laundromat.manager.access.securityquestion.mapper.SecurityQuestionEntitySelfMapper;
import com.teenthofabud.laundromat.manager.access.securityquestion.mapper.SecurityQuestionForm2EntityMapper;
import com.teenthofabud.laundromat.manager.access.securityquestion.repository.SecurityQuestionRepository;
import com.teenthofabud.laundromat.manager.access.securityquestion.service.SecurityQuestionService;
import com.teenthofabud.laundromat.manager.access.securityquestion.validator.SecurityQuestionDtoValidator;
import com.teenthofabud.laundromat.manager.access.securityquestion.validator.SecurityQuestionFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.access.securityquestion.validator.SecurityQuestionFormValidator;
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
public class SecurityQuestionServiceImpl implements SecurityQuestionService {

    private static final Comparator<SecurityQuestionVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private SecurityQuestionEntity2VoConverter entity2VoConverter;
    private SecurityQuestionForm2EntityConverter form2EntityConverter;
    private SecurityQuestionDto2EntityConverter dto2EntityConverter;
    private SecurityQuestionForm2EntityMapper form2EntityMapper;
    private SecurityQuestionEntitySelfMapper entitySelfMapper;
    private SecurityQuestionFormValidator formValidator;
    private SecurityQuestionFormRelaxedValidator relaxedFormValidator;
    private SecurityQuestionDtoValidator dtoValidator;
    private SecurityQuestionRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setEntity2VoConverter(SecurityQuestionEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(SecurityQuestionDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(SecurityQuestionForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(SecurityQuestionEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(SecurityQuestionFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchSecurityQuestionValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(SecurityQuestionDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(SecurityQuestionForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(SecurityQuestionRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(SecurityQuestionFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<SecurityQuestionVo> entity2DetailedVoList(List<SecurityQuestionEntity> securityQuestionEntityList) {
        List<SecurityQuestionVo> securityQuestionDetailsList = new ArrayList<>(securityQuestionEntityList.size());
        for(SecurityQuestionEntity entity : securityQuestionEntityList) {
            SecurityQuestionVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            securityQuestionDetailsList.add(vo);
        }
        return securityQuestionDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<SecurityQuestionVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all SecurityQuestionEntity by their natural ordering");
        List<SecurityQuestionEntity> securityQuestionEntityList = repository.findAll();
        Set<SecurityQuestionVo> naturallyOrderedSet = new TreeSet<SecurityQuestionVo>(CMP_BY_NAME);
        for(SecurityQuestionEntity entity : securityQuestionEntityList) {
            SecurityQuestionVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} SecurityQuestionVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public SecurityQuestionVo retrieveDetailsById(long id) throws SecurityQuestionException {
        log.info("Requesting SecurityQuestionEntity by id: {}", id);
        Optional<SecurityQuestionEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No SecurityQuestionEntity found by id: {}", id);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        SecurityQuestionEntity entity = optEntity.get();
        SecurityQuestionVo vo = entity2VoConverter.convert(entity);
        log.info("Found SecurityQuestionVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<SecurityQuestionVo> retrieveAllMatchingDetailsByName(String name) throws SecurityQuestionException {
        log.info("Requesting SecurityQuestionEntity that match with name: {}", name);
        List<SecurityQuestionEntity> securityQuestionEntityList = repository.findByNameContaining(name);
        if(securityQuestionEntityList != null && !securityQuestionEntityList.isEmpty()) {
            List<SecurityQuestionVo> matchedSecurityQuestionList = entity2DetailedVoList(securityQuestionEntityList);
            log.info("Found {} SecurityQuestionVo matching with name: {}", matchedSecurityQuestionList.size(),name);
            return matchedSecurityQuestionList;
        }
        log.debug("No SecurityQuestionVo found matching with name: {}", name);
        throw new SecurityQuestionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createSecurityQuestion(SecurityQuestionForm form) throws SecurityQuestionException {
        log.info("Creating new SecurityQuestionEntity");

        if(form == null) {
            log.debug("SecurityQuestionForm provided is null");
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of SecurityQuestionForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("SecurityQuestionForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("SecurityQuestionForm error detail: {}", ec);
            throw new SecurityQuestionException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of SecurityQuestionForm are valid");

        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_EXISTENCE_BY_NAME.getValue(), form.getName());
        SecurityQuestionEntity expectedEntity = form2EntityConverter.convert(form);
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        SecurityQuestionEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist SecurityQuestionForm details" });
        }
        log.info("Created new SecurityQuestionForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateSecurityQuestion(Long id, SecurityQuestionForm form) throws SecurityQuestionException {
        log.info("Updating SecurityQuestionForm by id: {}", id);

        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_SECURITY_QUESTION_ENTITY_ID.getValue(), id);
        Optional<SecurityQuestionEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_NO_SECURITY_QUESTION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_FOUND_SECURITY_QUESTION_ENTITY_ID.getValue(), id);

        SecurityQuestionEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("SecurityQuestionEntity is inactive with id: {}", id);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("SecurityQuestionEntity is active with id: {}", id);

        if(form == null) {
            log.debug("SecurityQuestionForm is null");
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of SecurityQuestionForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("SecurityQuestionForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("SecurityQuestionForm error detail: {}", ec);
            throw new SecurityQuestionException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of SecurityQuestionForm are empty");
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of SecurityQuestionForm are valid");

        Optional<SecurityQuestionEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of SecurityQuestionForm");
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from SecurityQuestionForm to SecurityQuestionEntity");

        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_EXISTENCE_BY_NAME.getValue(), form.getName());
        SecurityQuestionEntity expectedEntity = optExpectedEntity.get();
        if(actualEntity.getName().compareTo(expectedEntity.getName()) == 0 || repository.existsByName(expectedEntity.getName())) {
            log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from SecurityQuestionEntity to SecurityQuestionForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist currency securityQuestion details" });
        }
        log.info("Updated existing SecurityQuestionEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    public void deleteSecurityQuestion(Long id) throws SecurityQuestionException {
        log.info("Soft deleting SecurityQuestionEntity by id: {}", id);

        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_SECURITY_QUESTION_ENTITY_ID.getValue(), id);
        Optional<SecurityQuestionEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_NO_SECURITY_QUESTION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_FOUND_SECURITY_QUESTION_ENTITY_ID.getValue(), id);

        SecurityQuestionEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("SecurityQuestionEntity is inactive with id: {}", id);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("SecurityQuestionEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        SecurityQuestionEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current securityQuestion details with id:" + id });
        }

        log.info("Soft deleted existing SecurityQuestionEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnSecurityQuestion(Long id, List<PatchOperationForm> patches) throws SecurityQuestionException {
        log.info("Patching SecurityQuestionEntity by id: {}", id);

        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_SECURITY_QUESTION_ENTITY_ID.getValue(), id);
        Optional<SecurityQuestionEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_NO_SECURITY_QUESTION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_FOUND_SECURITY_QUESTION_ENTITY_ID.getValue(), id);

        SecurityQuestionEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("SecurityQuestion patch list not provided");
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("SecurityQuestion patch list has {} items", patches.size());


        log.debug("Validating patch list items for SecurityQuestion");
        try {
            toabBaseService.validatePatches(patches, AccessErrorCode.ACCESS_EXISTS.getDomain() + ":LOV");
            log.debug("All SecurityQuestion patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the SecurityQuestion patch item are invalid");
            throw new SecurityQuestionException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for SecurityQuestion");


        log.debug("Patching list items to SecurityQuestionDto");
        SecurityQuestionDto patchedSecurityQuestionForm = new SecurityQuestionDto();
        try {
            log.debug("Preparing patch list items for SecurityQuestion");
            JsonNode securityQuestionDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch securityQuestionPatch = JsonPatch.fromJson(securityQuestionDtoTree);
            log.debug("Prepared patch list items for SecurityQuestion");
            JsonNode blankSecurityQuestionDtoTree = om.convertValue(new SecurityQuestionDto(), JsonNode.class);
            JsonNode patchedSecurityQuestionFormTree = securityQuestionPatch.apply(blankSecurityQuestionDtoTree);
            log.debug("Applying patch list items to SecurityQuestionDto");
            patchedSecurityQuestionForm = om.treeToValue(patchedSecurityQuestionFormTree, SecurityQuestionDto.class);
            log.debug("Applied patch list items to SecurityQuestionDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to SecurityQuestionDto: {}", e);
            SecurityQuestionException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in SecurityQuestionDto");
                ex = new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new SecurityQuestionException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to SecurityQuestionDto: {}", e);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to SecurityQuestionDto");

        log.debug("Validating patched SecurityQuestionDto");
        Errors err = new DirectFieldBindingResult(patchedSecurityQuestionForm, patchedSecurityQuestionForm.getClass().getSimpleName());
        dtoValidator.validate(patchedSecurityQuestionForm, err);
        if(err.hasErrors()) {
            log.debug("Patched SecurityQuestionDto has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched SecurityQuestionDto error detail: {}", ec);
            throw new SecurityQuestionException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched SecurityQuestionDto are valid");

        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_EXISTENCE_BY_NAME.getValue(), patchedSecurityQuestionForm.getName().get());
        if(actualEntity.getName().compareTo(patchedSecurityQuestionForm.getName().get()) == 0 || repository.existsByName(patchedSecurityQuestionForm.getName().get())) {
            log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_EXISTS_BY_NAME.getValue(), patchedSecurityQuestionForm.getName().get());
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", patchedSecurityQuestionForm.getName().get() });
        }
        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_NON_EXISTENCE_BY_NAME.getValue(), patchedSecurityQuestionForm.getName().get());


        log.debug("Comparatively copying patched attributes from SecurityQuestionDto to SecurityQuestionEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedSecurityQuestionForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (SecurityQuestionException) e;
        }
        log.debug("Comparatively copied patched attributes from SecurityQuestionDto to SecurityQuestionEntity");

        log.debug("Saving patched SecurityQuestionEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched SecurityQuestionEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete SecurityQuestionEntity with id:{}", id);
            throw new SecurityQuestionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch currency securityQuestion details with id:" + id });
        }
        log.info("Patched SecurityQuestionEntity with id:{}", id);
    }
}