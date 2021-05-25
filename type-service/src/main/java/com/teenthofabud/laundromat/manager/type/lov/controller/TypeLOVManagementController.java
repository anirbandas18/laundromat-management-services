package com.teenthofabud.laundromat.manager.type.lov.controller;

import com.teenthofabud.core.common.model.form.PatchOperationForm;
import com.teenthofabud.laundromat.manager.constant.TypeSubDomain;
import com.teenthofabud.laundromat.manager.error.TypeErrorCode;
import com.teenthofabud.laundromat.manager.error.TypeException;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVForm;
import com.teenthofabud.laundromat.manager.type.lov.data.TypeLOVVo;
import com.teenthofabud.laundromat.manager.type.lov.service.TypeLOVService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Set;


@RestController
@RequestMapping("typelov")
public class TypeLOVManagementController {

    @Autowired
    public void setService(TypeLOVService service) {
        this.service = service;
    }

    private TypeLOVService service;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewTypeLOV(@RequestBody(required = false) TypeLOVForm form) throws TypeException {
        if(form != null) {
            long id = service.createTypeLOV(form);
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        throw new TypeException(TypeSubDomain.TYPE_LOV, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "not provided" });
    }

    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingTypeLOV(@PathVariable String id,
                                                           @RequestBody(required = false) TypeLOVForm form) throws TypeException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                if(form != null) {
                    service.updateTypeLOV(actualId, form);
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                throw new TypeException(TypeSubDomain.TYPE_LOV, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "form", "not provided" });
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        throw new TypeException(TypeSubDomain.TYPE_LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingTypeLOV(@PathVariable String id) throws TypeException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                service.deleteTypeLOV(actualId);
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        throw new TypeException(TypeSubDomain.TYPE_LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @PatchMapping(path = "{id}", consumes = "application/json-patch+json")
    public ResponseEntity<Void> patchExistingTypeLOV(@PathVariable String id,
                                                             @RequestBody(required = false) List<PatchOperationForm> dtoList) throws TypeException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                if(dtoList != null) {
                    service.applyPatchOnTypeLOV(actualId, dtoList);
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                throw new TypeException(TypeSubDomain.TYPE_LOV, TypeErrorCode.TYPE_ATTRIBUTE_UNEXPECTED, new Object[]{ "patch", "not provided" });
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        throw new TypeException(TypeSubDomain.TYPE_LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @GetMapping
    public Set<TypeLOVVo> getAllTypeLOVNaturallyOrdered() {
        Set<TypeLOVVo> naturallyOrderedStudents = service.retrieveAllByNaturalOrdering();
        return naturallyOrderedStudents;
    }

    @GetMapping("name/{name}")
    public List<TypeLOVVo> getAllStudentsByName(@PathVariable String name) throws TypeException {
        if(StringUtils.hasText(name)) {
            List<TypeLOVVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            return matchedByNames;
        }
        throw new TypeException(TypeSubDomain.TYPE_LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @GetMapping("id/{id}")
    public TypeLOVVo getTypeLOVDetailsById(@PathVariable String id) throws TypeException {
        if(StringUtils.hasText(id)) {
            try {
                long actualId = Long.parseLong(id);
                TypeLOVVo studentDetails = service.retrieveDetailsById(actualId);
                return studentDetails;
            } catch (NumberFormatException e) {
                e.printStackTrace();
            }
        }
        throw new TypeException(TypeSubDomain.TYPE_LOV, TypeErrorCode.TYPE_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
