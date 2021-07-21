package com.teenthofabud.laundromat.manager.access.role.service.impl;

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
import com.teenthofabud.laundromat.manager.access.role.converter.RoleDto2EntityConverter;
import com.teenthofabud.laundromat.manager.access.role.converter.RoleEntity2VoConverter;
import com.teenthofabud.laundromat.manager.access.role.converter.RoleForm2EntityConverter;
import com.teenthofabud.laundromat.manager.access.role.data.*;
import com.teenthofabud.laundromat.manager.access.role.mapper.RoleEntitySelfMapper;
import com.teenthofabud.laundromat.manager.access.role.mapper.RoleForm2EntityMapper;
import com.teenthofabud.laundromat.manager.access.role.repository.RoleRepository;
import com.teenthofabud.laundromat.manager.access.role.service.RoleService;
import com.teenthofabud.laundromat.manager.access.role.validator.RoleDtoValidator;
import com.teenthofabud.laundromat.manager.access.role.validator.RoleFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.access.role.validator.RoleFormValidator;
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
public class RoleServiceImpl implements RoleService {

    private static final Comparator<RoleVo> CMP_BY_NAME = (s1, s2) -> {
        return s1.getName().compareTo(s2.getName());
    };

    private RoleEntity2VoConverter entity2VoConverter;
    private RoleForm2EntityConverter form2EntityConverter;
    private RoleDto2EntityConverter dto2EntityConverter;
    private RoleForm2EntityMapper form2EntityMapper;
    private RoleEntitySelfMapper entitySelfMapper;
    private RoleFormValidator formValidator;
    private RoleFormRelaxedValidator relaxedFormValidator;
    private RoleDtoValidator dtoValidator;
    private RoleRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setEntity2VoConverter(RoleEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(RoleDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(RoleForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(RoleEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(RoleFormRelaxedValidator relaxedFormValidator) {
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
    public void setDtoValidator(RoleDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(RoleForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(RoleRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(RoleFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<RoleVo> entity2DetailedVoList(List<RoleEntity> roleEntityList) {
        List<RoleVo> roleDetailsList = new ArrayList<>(roleEntityList.size());
        for(RoleEntity entity : roleEntityList) {
            RoleVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            roleDetailsList.add(vo);
        }
        return roleDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<RoleVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all RoleEntity by their natural ordering");
        List<RoleEntity> roleEntityList = repository.findAll();
        Set<RoleVo> naturallyOrderedSet = new TreeSet<RoleVo>(CMP_BY_NAME);
        for(RoleEntity entity : roleEntityList) {
            RoleVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} RoleVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public RoleVo retrieveDetailsById(long id) throws RoleException {
        log.info("Requesting RoleEntity by id: {}", id);
        Optional<RoleEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No RoleEntity found by id: {}", id);
            throw new RoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        RoleEntity entity = optEntity.get();
        RoleVo vo = entity2VoConverter.convert(entity);
        log.info("Found RoleVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<RoleVo> retrieveAllMatchingDetailsByName(String name) throws RoleException {
        log.info("Requesting RoleEntity that match with name: {}", name);
        List<RoleEntity> roleEntityList = repository.findByNameContaining(name);
        if(roleEntityList != null && !roleEntityList.isEmpty()) {
            List<RoleVo> matchedRoleList = entity2DetailedVoList(roleEntityList);
            log.info("Found {} RoleVo matching with name: {}", matchedRoleList.size(),name);
            return matchedRoleList;
        }
        log.debug("No RoleVo found matching with name: {}", name);
        throw new RoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "name", name });
    }

    @Override
    @Transactional
    public Long createRole(RoleForm form) throws RoleException {
        log.info("Creating new RoleEntity");

        if(form == null) {
            log.debug("RoleForm provided is null");
            throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of RoleForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("RoleForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("RoleForm error detail: {}", ec);
            throw new RoleException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of RoleForm are valid");

        log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_EXISTENCE_BY_NAME.getValue(), form.getName());
        RoleEntity expectedEntity = form2EntityConverter.convert(form);
        if(repository.existsByName(expectedEntity.getName())) {
            log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new RoleException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", form.getName() });
        }
        log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        log.debug("Saving {}", expectedEntity);
        RoleEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new RoleException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist RoleForm details" });
        }
        log.info("Created new RoleForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateRole(Long id, RoleForm form) throws RoleException {
        log.info("Updating RoleForm by id: {}", id);

        log.debug(RoleMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ROLE_ENTITY_ID.getValue(), id);
        Optional<RoleEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(RoleMessageTemplate.MSG_TEMPLATE_NO_ROLE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new RoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(RoleMessageTemplate.MSG_TEMPLATE_FOUND_ROLE_ENTITY_ID.getValue(), id);

        RoleEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("RoleEntity is inactive with id: {}", id);
            throw new RoleException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("RoleEntity is active with id: {}", id);

        if(form == null) {
            log.debug("RoleForm is null");
            throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of RoleForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("RoleForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("RoleForm error detail: {}", ec);
            throw new RoleException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of RoleForm are empty");
            throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of RoleForm are valid");

        Optional<RoleEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
        if(optExpectedEntity.isEmpty()) {
            log.debug("No new value for attributes of RoleForm");
            throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
        }
        log.debug("Successfully compared and copied attributes from RoleForm to RoleEntity");

        log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_EXISTENCE_BY_NAME.getValue(), form.getName());
        RoleEntity expectedEntity = optExpectedEntity.get();
        if(actualEntity.getName().compareTo(expectedEntity.getName()) == 0 || repository.existsByName(expectedEntity.getName())) {
            log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_EXISTS_BY_NAME.getValue(), expectedEntity.getName());
            throw new RoleException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", actualEntity.getName() });
        }
        log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_NON_EXISTENCE_BY_NAME.getValue(), expectedEntity.getName());

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from RoleEntity to RoleForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new RoleException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist  role details" });
        }
        log.info("Updated existing RoleEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    //@Transactional(rollbackFor = AccessModelException.class, propagation = Propagation.REQUIRES_NEW, isolation = Isolation.READ_COMMITTED)
    public void deleteRole(Long id) throws RoleException {
        log.info("Soft deleting RoleEntity by id: {}", id);

        log.debug(RoleMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ROLE_ENTITY_ID.getValue(), id);
        Optional<RoleEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(RoleMessageTemplate.MSG_TEMPLATE_NO_ROLE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new RoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(RoleMessageTemplate.MSG_TEMPLATE_FOUND_ROLE_ENTITY_ID.getValue(), id);

        RoleEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("RoleEntity is inactive with id: {}", id);
            throw new RoleException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("RoleEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        RoleEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new RoleException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current role details with id:" + id });
        }

        log.info("Soft deleted existing RoleEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnRole(Long id, List<PatchOperationForm> patches) throws RoleException {
        log.info("Patching RoleEntity by id: {}", id);

        log.debug(RoleMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ROLE_ENTITY_ID.getValue(), id);
        Optional<RoleEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(RoleMessageTemplate.MSG_TEMPLATE_NO_ROLE_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new RoleException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(RoleMessageTemplate.MSG_TEMPLATE_FOUND_ROLE_ENTITY_ID.getValue(), id);

        RoleEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("Role patch list not provided");
            throw new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Role patch list has {} items", patches.size());


        log.debug("Validating patch list items for Role");
        try {
            toabBaseService.validatePatches(patches, AccessErrorCode.ACCESS_EXISTS.getDomain() + ":LOV");
            log.debug("All Role patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the Role patch item are invalid");
            throw new RoleException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for Role");


        log.debug("Patching list items to RoleDto");
        RoleDto patchedRoleForm = new RoleDto();
        try {
            log.debug("Preparing patch list items for Role");
            JsonNode roleDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch rolePatch = JsonPatch.fromJson(roleDtoTree);
            log.debug("Prepared patch list items for Role");
            JsonNode blankRoleDtoTree = om.convertValue(new RoleDto(), JsonNode.class);
            JsonNode patchedRoleFormTree = rolePatch.apply(blankRoleDtoTree);
            log.debug("Applying patch list items to RoleDto");
            patchedRoleForm = om.treeToValue(patchedRoleFormTree, RoleDto.class);
            log.debug("Applied patch list items to RoleDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to RoleDto: {}", e);
            RoleException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in RoleDto");
                ex = new RoleException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new RoleException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to RoleDto: {}", e);
            throw new RoleException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to RoleDto");

        log.debug("Validating patched RoleDto");
        Errors err = new DirectFieldBindingResult(patchedRoleForm, patchedRoleForm.getClass().getSimpleName());
        dtoValidator.validate(patchedRoleForm, err);
        if(err.hasErrors()) {
            log.debug("Patched RoleDto has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched RoleDto error detail: {}", ec);
            throw new RoleException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched RoleDto are valid");

        log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_EXISTENCE_BY_NAME.getValue(), patchedRoleForm.getName().get());
        if(actualEntity.getName().compareTo(patchedRoleForm.getName().get()) == 0 || repository.existsByName(patchedRoleForm.getName().get())) {
            log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_EXISTS_BY_NAME.getValue(), patchedRoleForm.getName().get());
            throw new RoleException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "name", patchedRoleForm.getName().get() });
        }
        log.debug(RoleMessageTemplate.MSG_TEMPLATE_ROLE_NON_EXISTENCE_BY_NAME.getValue(), patchedRoleForm.getName().get());


        log.debug("Comparatively copying patched attributes from RoleDto to RoleEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedRoleForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (RoleException) e;
        }
        log.debug("Comparatively copied patched attributes from RoleDto to RoleEntity");

        log.debug("Saving patched RoleEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched RoleEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete RoleEntity with id:{}", id);
            throw new RoleException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch  role details with id:" + id });
        }
        log.info("Patched RoleEntity with id:{}", id);
    }
}