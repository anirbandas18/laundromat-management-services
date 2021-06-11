package com.teenthofabud.laundromat.manager.type.lov.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.type.constant.TypeSubDomain;
import com.teenthofabud.laundromat.manager.type.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.type.error.TypeException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVForm;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import com.teenthofabud.laundromat.manager.type.lov.service.TypeLOVService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("lov")
@Slf4j
public class TypeLOVController {

    @Autowired
    public void setService(TypeLOVService service) {
        this.service = service;
    }

    private TypeLOVService service;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewTypeLOV(@RequestBody(required = false) TypeLOVForm form) throws TypeException {
        log.debug("Requesting to create new type LOV");
        if(form != null) {
            Long id = service.createTypeLOV(form);
            log.debug("Responding with identifier of newly created new type LOV");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("TypeLOVForm is null");
        throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingTypeLOV(@PathVariable String id, @RequestBody(required = false) TypeLOVForm form) throws TypeException {
        log.debug("Requesting to update all attributes of existing type LOV");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug("type LOV id: {} is semantically valid", id);
                if(form != null) {
                    service.updateTypeLOV(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing type LOV");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("TypeLOVForm is null");
                throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug("type LOV id: {} is invalid", id);
                throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug("type LOV id is empty");
        throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingTypeLOV(@PathVariable String id) throws TypeException {
        log.debug("Requesting to soft delete type LOV");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug("type LOV id: {} is semantically valid", id);
                service.deleteTypeLOV(actualId);
                log.debug("Responding with successful deletion of existing type LOV");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug("type LOV id: {} is invalid", id);
                throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug("type LOV id is empty");
        throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @PatchMapping(path = "{id}", consumes = "application/json-patch+json")
    public ResponseEntity<Void> patchExistingTypeLOV(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws TypeException {
        log.debug("Requesting to patch of type LOV attributes");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug("type LOV id: {} is semantically valid", id);
                if(dtoList != null) {
                    service.applyPatchOnTypeLOV(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing type LOV");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("type LOV patch document is null");
                throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug("type LOV id: {} is invalid", id);
                throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug("type LOV id is empty");
        throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @GetMapping
    public Set<TypeLOVVo> getAllTypeLOVNaturallyOrdered() {
        log.debug("Requesting all available type LOVs by their natural orders");
        Set<TypeLOVVo> naturallyOrderedStudents = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available type LOVs by their natural orders");
        return naturallyOrderedStudents;
    }

    @GetMapping("name/{name}")
    public List<TypeLOVVo> getAllStudentsByName(@PathVariable String name) throws TypeException {
        log.debug("Requesting all available type LOVs with given name");
        if(StringUtils.hasText(name)) {
            List<TypeLOVVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available type LOVs with given name");
            return matchedByNames;
        }
        log.debug("type LOV name is empty");
        throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @GetMapping("{id}")
    public TypeLOVVo getTypeLOVDetailsById(@PathVariable String id) throws TypeException {
        log.debug("Requesting all available type LOVs by its id");
        if(StringUtils.hasText(id)) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug("type LOV id: {} is semantically valid", id);
                TypeLOVVo studentDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing type LOV details by id");
                return studentDetails;
            } catch (NumberFormatException e) {
                log.debug("type LOV id: {} is invalid", id);
                throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug("type LOV id is empty");
        throw new TypeException(TypeSubDomain.LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
