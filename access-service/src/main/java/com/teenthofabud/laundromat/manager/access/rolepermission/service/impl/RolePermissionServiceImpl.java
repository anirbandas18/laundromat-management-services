package com.teenthofabud.laundromat.manager.access.rolepermission.service.impl;

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
import com.teenthofabud.laundromat.manager.access.rolepermission.converter.RolePermissionDto2EntityConverter;
import com.teenthofabud.laundromat.manager.access.rolepermission.converter.RolePermissionEntity2VoConverter;
import com.teenthofabud.laundromat.manager.access.rolepermission.converter.RolePermissionForm2EntityConverter;
import com.teenthofabud.laundromat.manager.access.rolepermission.data.*;
import com.teenthofabud.laundromat.manager.access.rolepermission.mapper.RolePermissionEntitySelfMapper;
import com.teenthofabud.laundromat.manager.access.rolepermission.mapper.RolePermissionForm2EntityMapper;
import com.teenthofabud.laundromat.manager.access.rolepermission.repository.RolePermissionRepository;
import com.teenthofabud.laundromat.manager.access.rolepermission.service.RolePermissionService;
import com.teenthofabud.laundromat.manager.access.rolepermission.validator.RolePermissionDtoValidator;
import com.teenthofabud.laundromat.manager.access.rolepermission.validator.RolePermissionFormRelaxedValidator;
import com.teenthofabud.laundromat.manager.access.rolepermission.validator.RolePermissionFormValidator;
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
public class RolePermissionServiceImpl implements RolePermissionService {

    private static final Comparator<RolePermissionVo> CMP_BY_ID = (s1, s2) -> {
        return Long.compare(s1.getId(), s2.getId());
    };

    private RolePermissionEntity2VoConverter entity2VoConverter;
    private RolePermissionForm2EntityConverter form2EntityConverter;
    private RolePermissionDto2EntityConverter dto2EntityConverter;
    private RolePermissionForm2EntityMapper form2EntityMapper;
    private RolePermissionEntitySelfMapper entitySelfMapper;
    private RolePermissionFormValidator formValidator;
    private RolePermissionFormRelaxedValidator relaxedFormValidator;
    private RolePermissionDtoValidator dtoValidator;
    private RolePermissionRepository repository;
    private TOABBaseService toabBaseService;
    private ObjectMapper om;

    @Autowired
    public void setEntity2VoConverter(RolePermissionEntity2VoConverter entity2VoConverter) {
        this.entity2VoConverter = entity2VoConverter;
    }

    @Autowired
    public void setDto2EntityConverter(RolePermissionDto2EntityConverter dto2EntityConverter) {
        this.dto2EntityConverter = dto2EntityConverter;
    }

    @Autowired
    public void setForm2EntityMapper(RolePermissionForm2EntityMapper form2EntityMapper) {
        this.form2EntityMapper = form2EntityMapper;
    }

    @Autowired
    public void setEntitySelfMapper(RolePermissionEntitySelfMapper entitySelfMapper) {
        this.entitySelfMapper = entitySelfMapper;
    }

    @Autowired
    public void setRelaxedFormValidator(RolePermissionFormRelaxedValidator relaxedFormValidator) {
        this.relaxedFormValidator = relaxedFormValidator;
    }

    @Autowired
    public void setPatchRolePermissionValidator(TOABBaseService toabBaseService) {
        this.toabBaseService = toabBaseService;
    }

    @Autowired
    public void setOm(ObjectMapper om) {
        this.om = om;
    }

    @Autowired
    public void setDtoValidator(RolePermissionDtoValidator dtoValidator) {
        this.dtoValidator = dtoValidator;
    }

    @Autowired
    public void setForm2EntityConverter(RolePermissionForm2EntityConverter form2EntityConverter) {
        this.form2EntityConverter = form2EntityConverter;
    }

    @Autowired
    public void setRepository(RolePermissionRepository repository) {
        this.repository = repository;
    }

    @Autowired
    public void setFormValidator(RolePermissionFormValidator formValidator) {
        this.formValidator = formValidator;
    }

    private List<RolePermissionVo> entity2DetailedVoList(List<RolePermissionEntity> rolePermissionEntityList) {
        List<RolePermissionVo> rolePermissionDetailsList = new ArrayList<>(rolePermissionEntityList.size());
        for(RolePermissionEntity entity : rolePermissionEntityList) {
            RolePermissionVo vo = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, vo);
            rolePermissionDetailsList.add(vo);
        }
        return rolePermissionDetailsList;
    }

    @Override
    @Transactional(readOnly = true)
    public Set<RolePermissionVo> retrieveAllByNaturalOrdering() {
        log.info("Requesting all RolePermissionEntity by their natural ordering");
        List<RolePermissionEntity> rolePermissionEntityList = repository.findAll();
        Set<RolePermissionVo> naturallyOrderedSet = new TreeSet<RolePermissionVo>(CMP_BY_ID);
        for(RolePermissionEntity entity : rolePermissionEntityList) {
            RolePermissionVo dto = entity2VoConverter.convert(entity);
            log.debug("Converting {} to {}", entity, dto);
            naturallyOrderedSet.add(dto);
        }
        log.info("{} RolePermissionVo available", naturallyOrderedSet.size());
        return naturallyOrderedSet;
    }

    @Override
    @Transactional(readOnly = true)
    public RolePermissionVo retrieveDetailsById(Long id) throws RolePermissionException {
        log.info("Requesting RolePermissionEntity by id: {}", id);
        Optional<RolePermissionEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug("No RolePermissionEntity found by id: {}", id);
            throw new RolePermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        RolePermissionEntity entity = optEntity.get();
        RolePermissionVo vo = entity2VoConverter.convert(entity);
        log.info("Found RolePermissionVo by id: {}", id);
        return vo;
    }


    @Override
    @Transactional(readOnly = true)
    public List<RolePermissionVo> retrieveAllMatchingDetailsByPermission(Long permissionId) throws RolePermissionException {
        log.info("Requesting RolePermissionEntity that match with permissionId: {}", permissionId);
        List<RolePermissionEntity> rolePermissionEntityList = repository.findByPermissionId(permissionId);
        if(rolePermissionEntityList != null && !rolePermissionEntityList.isEmpty()) {
            List<RolePermissionVo> matchedRolePermissionList = entity2DetailedVoList(rolePermissionEntityList);
            log.info("Found {} RolePermissionVo matching with permissionId: {}", matchedRolePermissionList.size(),permissionId);
            return matchedRolePermissionList;
        }
        log.debug("No RolePermissionVo found matching with permissionId: {}", permissionId);
        throw new RolePermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "permissionId", permissionId });
    }

    @Override
    @Transactional(readOnly = true)
    public List<RolePermissionVo> retrieveAllMatchingDetailsByRole(Long roleId) throws RolePermissionException {
        log.info("Requesting RolePermissionEntity that match with roleId: {}", roleId);
        List<RolePermissionEntity> rolePermissionEntityList = repository.findByRoleId(roleId);
        if(rolePermissionEntityList != null && !rolePermissionEntityList.isEmpty()) {
            List<RolePermissionVo> matchedRolePermissionList = entity2DetailedVoList(rolePermissionEntityList);
            log.info("Found {} RolePermissionVo matching with roleId: {}", matchedRolePermissionList.size(),roleId);
            return matchedRolePermissionList;
        }
        log.debug("No RolePermissionVo found matching with roleId: {}", roleId);
        throw new RolePermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "roleId", roleId });
    }

    @Override
    @Transactional
    public Long createRolePermission(RolePermissionForm form) throws RolePermissionException {
        log.info("Creating new RolePermissionEntity");

        if(form == null) {
            log.debug("RolePermissionForm provided is null");
            throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                    new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details: {}", form);

        log.debug("Validating provided attributes of RolePermissionForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        formValidator.validate(form, err);
        if(err.hasErrors()) {
            log.debug("RolePermissionForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("RolePermissionForm error detail: {}", ec);
            throw new RolePermissionException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of RolePermissionForm are valid");

        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                form.getPermissionId(), form.getRoleId());
        if(repository.existsByPermissionIdAndRoleId(form.getPermissionId(), form.getRoleId())) {
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTS_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    form.getPermissionId(), form.getRoleId());
            throw new RolePermissionException(AccessErrorCode.ACCESS_EXISTS,
                    new Object[]{ "permissionId: " + form.getPermissionId(), "roleId: " + form.getRoleId() });
        }
        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_NON_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                form.getPermissionId(), form.getRoleId());

        RolePermissionEntity expectedEntity = form2EntityConverter.convert(form);
        log.debug("Saving {}", expectedEntity);
        RolePermissionEntity actualEntity = repository.save(expectedEntity);
        log.debug("Saved {}", actualEntity);

        if(actualEntity == null) {
            log.debug("Unable to create {}", expectedEntity);
            throw new RolePermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "creation", "unable to persist RolePermissionForm details" });
        }
        log.info("Created new RolePermissionForm with id: {}", actualEntity.getId());
        return actualEntity.getId();
    }

    @Override
    @Transactional
    public void updateRolePermission(Long id, RolePermissionForm form) throws RolePermissionException {
        log.info("Updating RolePermissionForm by id: {}", id);

        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ROLE_PERMISSION_ENTITY_ID.getValue(), id);
        Optional<RolePermissionEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_NO_ROLE_PERMISSION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new RolePermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_FOUND_ROLE_PERMISSION_ENTITY_ID.getValue(), id);

        RolePermissionEntity actualEntity = optActualEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("RolePermissionEntity is inactive with id: {}", id);
            throw new RolePermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("RolePermissionEntity is active with id: {}", id);

        if(form == null) {
            log.debug("RolePermissionForm is null");
            throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("Form details : {}", form);

        log.debug("Validating provided attributes of RolePermissionForm");
        Errors err = new DirectFieldBindingResult(form, form.getClass().getSimpleName());
        Boolean allEmpty = relaxedFormValidator.validateLoosely(form, err);
        if(err.hasErrors()) {
            log.debug("RolePermissionForm has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("RolePermissionForm error detail: {}", ec);
            throw new RolePermissionException(ec, new Object[] { err.getFieldError().getField(), err.getFieldError().getCode() });
        } else if (!allEmpty) {
            log.debug("All attributes of RolePermissionForm are empty");
            throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are empty" });
        }
        log.debug("All attributes of RolePermissionForm are valid");

        RolePermissionEntity expectedEntity = null;

        try {
            Optional<RolePermissionEntity> optExpectedEntity = form2EntityMapper.compareAndMap(actualEntity, form);
            if(optExpectedEntity.isEmpty()) {
                log.debug("No new value for attributes of RolePermissionForm");
                throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "fields are expected with new values" });
            }
            log.debug("Successfully compared and copied attributes from RolePermissionForm to RolePermissionEntity");
            expectedEntity = optExpectedEntity.get();
        } catch (TOABBaseException e) {
            throw (RolePermissionException) e;
        }

        checkUniquenessOfRolePermissionModel(form, actualEntity);

        entitySelfMapper.compareAndMap(expectedEntity, actualEntity);
        log.debug("Compared and copied attributes from RolePermissionEntity to RolePermissionForm");
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));

        log.debug("Updating: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Updated: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to update {}", actualEntity);
            throw new RolePermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "update", "unable to persist  rolePermission details" });
        }
        log.info("Updated existing RolePermissionEntity with id: {} to version: {}", actualEntity.getId(), actualEntity.getVersion());
    }

    @Override
    @Transactional
    public void deleteRolePermission(Long id) throws RolePermissionException {
        log.info("Soft deleting RolePermissionEntity by id: {}", id);

        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ROLE_PERMISSION_ENTITY_ID.getValue(), id);
        Optional<RolePermissionEntity> optEntity = repository.findById(id);
        if(optEntity.isEmpty()) {
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_NO_ROLE_PERMISSION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new RolePermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_FOUND_ROLE_PERMISSION_ENTITY_ID.getValue(), id);

        RolePermissionEntity actualEntity = optEntity.get();
        if(!actualEntity.getActive()) {
            log.debug("RolePermissionEntity is inactive with id: {}", id);
            throw new RolePermissionException(AccessErrorCode.ACCESS_INACTIVE, new Object[] { String.valueOf(id) });
        }
        log.debug("RolePermissionEntity is active with id: {}", id);

        actualEntity.setActive(Boolean.FALSE);
        actualEntity.setModifiedOn(LocalDateTime.now(ZoneOffset.UTC));
        log.debug("Soft deleting: {}", actualEntity);
        RolePermissionEntity expectedEntity = repository.save(actualEntity);
        log.debug("Soft deleted: {}", expectedEntity);
        if(expectedEntity == null) {
            log.debug("Unable to soft delete {}", actualEntity);
            throw new RolePermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "deletion", "unable to soft delete current rolePermission details with id:" + id });
        }

        log.info("Soft deleted existing RolePermissionEntity with id: {}", actualEntity.getId());
    }

    @Override
    @Transactional
    public void applyPatchOnRolePermission(Long id, List<PatchOperationForm> patches) throws RolePermissionException {
        log.info("Patching RolePermissionEntity by id: {}", id);

        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_SEARCHING_FOR_ROLE_PERMISSION_ENTITY_ID.getValue(), id);
        Optional<RolePermissionEntity> optActualEntity = repository.findById(id);
        if(optActualEntity.isEmpty()) {
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_NO_ROLE_PERMISSION_ENTITY_ID_AVAILABLE.getValue(), id);
            throw new RolePermissionException(AccessErrorCode.ACCESS_NOT_FOUND, new Object[] { "id", String.valueOf(id) });
        }
        log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_FOUND_ROLE_PERMISSION_ENTITY_ID.getValue(), id);

        RolePermissionEntity actualEntity = optActualEntity.get();
        if(patches == null || (patches != null && patches.isEmpty())) {
            log.debug("RolePermission patch list not provided");
            throw new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
        }
        log.debug("RolePermission patch list has {} items", patches.size());


        log.debug("Validating patch list items for RolePermission");
        try {
            toabBaseService.validatePatches(patches, AccessErrorCode.ACCESS_EXISTS.getDomain() + ":LOV");
            log.debug("All RolePermission patch list items are valid");
        } catch (TOABSystemException e) {
            log.debug("Some of the RolePermission patch item are invalid");
            throw new RolePermissionException(e.getError(), e.getParameters());
        }
        log.debug("Validated patch list items for RolePermission");


        log.debug("Patching list items to RolePermissionDto");
        RolePermissionDto patchedRolePermissionForm = new RolePermissionDto();
        try {
            log.debug("Preparing patch list items for RolePermission");
            JsonNode rolePermissionDtoTree = om.convertValue(patches, JsonNode.class);
            JsonPatch rolePermissionPatch = JsonPatch.fromJson(rolePermissionDtoTree);
            log.debug("Prepared patch list items for RolePermission");
            JsonNode blankRolePermissionDtoTree = om.convertValue(new RolePermissionDto(), JsonNode.class);
            JsonNode patchedRolePermissionFormTree = rolePermissionPatch.apply(blankRolePermissionDtoTree);
            log.debug("Applying patch list items to RolePermissionDto");
            patchedRolePermissionForm = om.treeToValue(patchedRolePermissionFormTree, RolePermissionDto.class);
            log.debug("Applied patch list items to RolePermissionDto");
        } catch (JsonPatchException e) {
            log.debug("Failed to patch list items to RolePermissionDto: {}", e);
            RolePermissionException ex = null;
            if(e.getMessage().contains("no such path in target")) {
                log.debug("Invalid patch attribute in RolePermissionDto");
                ex = new RolePermissionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[]{ "path" });
            } else {
                ex = new RolePermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
            }
            throw ex;
        } catch (IOException e) {
            log.debug("Failed to patch list items to RolePermissionDto: {}", e);
            throw new RolePermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE, new Object[]{ "patching", "internal error: " + e.getMessage() });
        }
        log.debug("Successfully to patch list items to RolePermissionDto");

        log.debug("Validating patched RolePermissionDto");
        Errors err = new DirectFieldBindingResult(patchedRolePermissionForm, patchedRolePermissionForm.getClass().getSimpleName());
        dtoValidator.validate(patchedRolePermissionForm, err);
        if(err.hasErrors()) {
            log.debug("Patched RolePermissionDto has {} errors", err.getErrorCount());
            AccessErrorCode ec = AccessErrorCode.valueOf(err.getFieldError().getCode());
            log.debug("Patched RolePermissionDto error detail: {}", ec);
            throw new RolePermissionException(ec, new Object[] { err.getFieldError().getField() });
        }
        log.debug("All attributes of patched RolePermissionDto are valid");

        checkUniquenessOfRolePermissionModel(patchedRolePermissionForm, actualEntity);

        log.debug("Comparatively copying patched attributes from RolePermissionDto to RolePermissionEntity");
        try {
            dto2EntityConverter.compareAndMap(patchedRolePermissionForm, actualEntity);
        } catch (TOABBaseException e) {
            throw (RolePermissionException) e;
        }
        log.debug("Comparatively copied patched attributes from RolePermissionDto to RolePermissionEntity");

        log.debug("Saving patched RolePermissionEntity: {}", actualEntity);
        actualEntity = repository.save(actualEntity);
        log.debug("Saved patched RolePermissionEntity: {}", actualEntity);
        if(actualEntity == null) {
            log.debug("Unable to patch delete RolePermissionEntity with id:{}", id);
            throw new RolePermissionException(AccessErrorCode.ACCESS_ACTION_FAILURE,
                    new Object[]{ "patching", "unable to patch  rolePermission details with id:" + id });
        }
        log.info("Patched RolePermissionEntity with id:{}", id);
    }

    private void checkUniquenessOfRolePermissionModel(RolePermissionDto patchedRolePermissionForm, RolePermissionEntity actualEntity) throws RolePermissionException {
        // permissionId = true, roleId = false
        if(patchedRolePermissionForm.getPermissionId().isPresent() && patchedRolePermissionForm.getRoleId().isEmpty()) {
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    patchedRolePermissionForm.getPermissionId().get(), actualEntity.getRole().getId());
            Long permissionId = Long.parseLong(patchedRolePermissionForm.getPermissionId().get());
            boolean duplicateEntitySw =  repository.existsByPermissionIdAndRoleId(permissionId, actualEntity.getRole().getId());
            if(duplicateEntitySw) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTS_BY_PERMISSION_ID_ROLE_ID.getValue(),
                        patchedRolePermissionForm.getPermissionId().get(), actualEntity.getRole().getId());
                throw new RolePermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "permissionId " + patchedRolePermissionForm.getPermissionId().get(), "roleId " + actualEntity.getRole().getId() });
            }
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_NON_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    patchedRolePermissionForm.getPermissionId().get(), actualEntity.getRole().getId());
        }

        // permissionId = false, roleId = true
        if(patchedRolePermissionForm.getPermissionId().isEmpty() && patchedRolePermissionForm.getRoleId().isPresent()) {
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    actualEntity.getPermission().getId(), patchedRolePermissionForm.getRoleId().get());
            Long roleId = Long.parseLong(patchedRolePermissionForm.getRoleId().get());
            boolean duplicateEntitySw =  repository.existsByPermissionIdAndRoleId(actualEntity.getPermission().getId(), roleId);
            if(duplicateEntitySw) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTS_BY_PERMISSION_ID_ROLE_ID.getValue(),
                        actualEntity.getPermission().getId(), patchedRolePermissionForm.getRoleId().get());
                throw new RolePermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "permissionId " + actualEntity.getPermission().getId(), "roleId " + patchedRolePermissionForm.getRoleId().get() });
            }
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_NON_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    actualEntity.getPermission().getId(), patchedRolePermissionForm.getRoleId().get());
        }

        // permissionId = true, roleId = true
        if(patchedRolePermissionForm.getPermissionId().isEmpty() && patchedRolePermissionForm.getRoleId().isPresent()) {
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    patchedRolePermissionForm.getPermissionId().get(), patchedRolePermissionForm.getRoleId().get());
            Long permissionId = Long.parseLong(patchedRolePermissionForm.getPermissionId().get());
            Long roleId = Long.parseLong(patchedRolePermissionForm.getRoleId().get());
            boolean duplicateEntitySw =  repository.existsByPermissionIdAndRoleId(permissionId, roleId);
            if(duplicateEntitySw) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTS_BY_PERMISSION_ID_ROLE_ID.getValue(),
                        patchedRolePermissionForm.getPermissionId().get(), patchedRolePermissionForm.getRoleId().get());
                throw new RolePermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "permissionId " + patchedRolePermissionForm.getPermissionId().get(), "roleId " + patchedRolePermissionForm.getRoleId().get() });
            }
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_NON_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    patchedRolePermissionForm.getPermissionId().get(), patchedRolePermissionForm.getRoleId().get());
        }
    }

    private void checkUniquenessOfRolePermissionModel(RolePermissionForm rolePermissionForm, RolePermissionEntity actualEntity) throws RolePermissionException {
        // permissionId = true, roleId = false
        if(rolePermissionForm.getPermissionId() != null && rolePermissionForm.getRoleId() == null) {
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    rolePermissionForm.getPermissionId(), actualEntity.getRole().getId());
            boolean duplicateEntitySw =  repository.existsByPermissionIdAndRoleId(rolePermissionForm.getPermissionId(), actualEntity.getRole().getId());
            if(duplicateEntitySw) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTS_BY_PERMISSION_ID_ROLE_ID.getValue(),
                        rolePermissionForm.getPermissionId(), actualEntity.getRole().getId());
                throw new RolePermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "permissionId " + rolePermissionForm.getPermissionId(), "roleId " + actualEntity.getRole().getId() });
            }
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_NON_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    rolePermissionForm.getPermissionId(), actualEntity.getRole().getId());
        }

        // permissionId = false, roleId = true
        if(rolePermissionForm.getPermissionId() == null && rolePermissionForm.getRoleId() != null) {
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    actualEntity.getPermission().getId(), rolePermissionForm.getRoleId());
            boolean duplicateEntitySw =  repository.existsByPermissionIdAndRoleId(actualEntity.getPermission().getId(), rolePermissionForm.getRoleId());
            if(duplicateEntitySw) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTS_BY_PERMISSION_ID_ROLE_ID.getValue(),
                        actualEntity.getPermission().getId(), rolePermissionForm.getRoleId());
                throw new RolePermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "permissionId " + actualEntity.getPermission().getId(), "roleId " + rolePermissionForm.getRoleId() });
            }
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_NON_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    actualEntity.getPermission().getId(), rolePermissionForm.getRoleId());
        }

        // permissionId = true, roleId = true
        if(rolePermissionForm.getPermissionId() != null && rolePermissionForm.getRoleId() != null) {
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    rolePermissionForm.getPermissionId(), rolePermissionForm.getRoleId());
            boolean duplicateEntitySw =  repository.existsByPermissionIdAndRoleId(rolePermissionForm.getPermissionId(), rolePermissionForm.getRoleId());
            if(duplicateEntitySw) {
                log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_EXISTS_BY_PERMISSION_ID_ROLE_ID.getValue(),
                        rolePermissionForm.getPermissionId(), rolePermissionForm.getRoleId());
                throw new RolePermissionException(AccessErrorCode.ACCESS_EXISTS,
                        new Object[]{ "permissionId " + rolePermissionForm.getPermissionId(), "roleId " + rolePermissionForm.getRoleId() });
            }
            log.debug(RolePermissionMessageTemplate.MSG_TEMPLATE_ROLE_PERMISSION_NON_EXISTENCE_BY_PERMISSION_ID_ROLE_ID.getValue(),
                    rolePermissionForm.getPermissionId(), rolePermissionForm.getRoleId());
        }
    }

}