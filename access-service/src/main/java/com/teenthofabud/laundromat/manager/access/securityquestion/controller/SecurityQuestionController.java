package com.teenthofabud.laundromat.manager.access.securityquestion.controller;

import com.teenthofabud.core.common.constant.TOABBaseMessageTemplate;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionException;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionForm;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionMessageTemplate;
import com.teenthofabud.laundromat.manager.access.securityquestion.data.SecurityQuestionVo;
import com.teenthofabud.laundromat.manager.access.securityquestion.service.SecurityQuestionService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@RequestMapping("securityquestion")
@Slf4j
@Tag(name = "SecurityQuestion API", description = "Manage SecurityQuestions and their details")
public class SecurityQuestionController {

    private static final String MEDIA_ACCESS_APPLICATION_JSON_PATCH = "application/json-patch+json";

    @Autowired
    public void setService(SecurityQuestionService service) {
        this.service = service;
    }

    private SecurityQuestionService service;

    @Operation(summary = "Create new SecurityQuestion details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "201", description = "Id of newly created SecurityQuestion",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = String.class)) }),
            @ApiResponse(responseCode = "400", description = "SecurityQuestion attribute's value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "SecurityQuestion already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No SecurityQuestion attributes provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to create new SecurityQuestion",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.CREATED)
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Long> postNewSecurityQuestion(@RequestBody(required = false) SecurityQuestionForm form) throws SecurityQuestionException {
        log.debug("Requesting to create new operation");
        if(form != null) {
            Long id = service.createSecurityQuestion(form);
            log.debug("Responding with identifier of newly created new operation");
            return ResponseEntity.status(HttpStatus.CREATED).body(id);
        }
        log.debug("SecurityQuestionForm is null");
        throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
    }

    @Operation(summary = "Update SecurityQuestion details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Updated details of SecurityQuestion",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "SecurityQuestion attribute's value is invalid/SecurityQuestion is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No SecurityQuestion found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "409", description = "SecurityQuestion already exists with the given attribute values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to update SecurityQuestion details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PutMapping(path = "{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Void> putExistingSecurityQuestion(@PathVariable String id, @RequestBody(required = false) SecurityQuestionForm form) throws SecurityQuestionException {
        log.debug("Requesting to update all attributes of existing operation");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_VALID.getValue(), id);
                if(form != null) {
                    service.updateSecurityQuestion(actualId, form);
                    log.debug("Responding with successful updation of attributes for existing operation");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("SecurityQuestionForm is null");
                throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "form", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_INVALID.getValue(), id);
                throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_EMPTY.getValue());
        throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Soft delete SecurityQuestion by id and all associated Type Models")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Soft deleted SecurityQuestion",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "SecurityQuestion id is invalid/SecurityQuestion is inactive",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No SecurityQuestion found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No SecurityQuestion attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to soft delete SecurityQuestion",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @DeleteMapping("{id}")
    public ResponseEntity<Void> deleteExistingSecurityQuestion(@PathVariable String id) throws SecurityQuestionException {
        log.debug("Requesting to soft delete operation");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_VALID.getValue(), id);
                service.deleteSecurityQuestion(actualId);
                log.debug("Responding with successful deletion of existing operation");
                return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
            } catch (NumberFormatException e) {
                log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_INVALID.getValue(), id);
                throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_EMPTY.getValue());
        throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Patch SecurityQuestion attributes by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "204", description = "Patched each provided attribute of SecurityQuestion with the given value",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = Void.class)) }),
            @ApiResponse(responseCode = "400", description = "SecurityQuestion attribute/value is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No SecurityQuestion found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "422", description = "No SecurityQuestion attribute patches provided",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "500", description = "Internal system error while trying to patch provided attributes of SecurityQuestion with the given values",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.NO_CONTENT)
    @PatchMapping(path = "{id}", consumes = MEDIA_ACCESS_APPLICATION_JSON_PATCH)
    public ResponseEntity<Void> patchExistingSecurityQuestion(@PathVariable String id, @RequestBody(required = false) List<PatchOperationForm> dtoList)
            throws SecurityQuestionException {
        log.debug("Requesting to patch of operation attributes");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_VALID.getValue(), id);
                if(dtoList != null) {
                    service.applyPatchOnSecurityQuestion(actualId, dtoList);
                    log.debug("Responding with successful patch of attributes for existing operation");
                    return ResponseEntity.status(HttpStatus.NO_CONTENT).build();
                }
                log.debug("operation patch document is null");
                throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED,
                        new Object[]{ "patch", TOABBaseMessageTemplate.MSG_TEMPLATE_NOT_PROVIDED });
            } catch (NumberFormatException e) {
                log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_INVALID.getValue(), id);
                throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_EMPTY.getValue());
        throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

    @Operation(summary = "Get all SecurityQuestion details")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve all available SecurityQuestions and their details",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = SecurityQuestionVo.class))) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping
    public Set<SecurityQuestionVo> getAllSecurityQuestionNaturallyOrdered() {
        log.debug("Requesting all available operations by their natural orders");
        Set<SecurityQuestionVo> naturallyOrderedSecurityQuestions = service.retrieveAllByNaturalOrdering();
        log.debug("Responding with all available operations by their natural orders");
        return naturallyOrderedSecurityQuestions;
    }

    @Operation(summary = "Get all SecurityQuestion details by name")
    @ApiResponses(value = {
        @ApiResponse(responseCode = "200", description = "Retrieve all available SecurityQuestions and their details that match the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, array = @ArraySchema(schema = @Schema(implementation = SecurityQuestionVo.class))) }),
        @ApiResponse(responseCode = "400", description = "SecurityQuestion name is invalid",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
        @ApiResponse(responseCode = "404", description = "No SecurityQuestions available with the given name",
            content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("name/{name}")
    public List<SecurityQuestionVo> getAllSecurityQuestionsByName(@PathVariable String name) throws SecurityQuestionException {
        log.debug("Requesting all available operations with given name");
        if(StringUtils.hasText(StringUtils.trimWhitespace(name))) {
            List<SecurityQuestionVo> matchedByNames = service.retrieveAllMatchingDetailsByName(name);
            log.debug("Responding with all available operations with given name");
            return matchedByNames;
        }
        log.debug("operation name is empty");
        throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "name", name });
    }

    @Operation(summary = "Get SecurityQuestion details by id")
    @ApiResponses(value = {
            @ApiResponse(responseCode = "200", description = "Retrieve the details of SecurityQuestion that matches the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = SecurityQuestionVo.class)) }),
            @ApiResponse(responseCode = "400", description = "SecurityQuestion id is invalid",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) }),
            @ApiResponse(responseCode = "404", description = "No SecurityQuestion found with the given id",
                    content = { @Content(mediaType = MediaType.APPLICATION_JSON_VALUE, schema = @Schema(implementation = ErrorVo.class)) })
    })
    @ResponseStatus(HttpStatus.OK)
    @GetMapping("{id}")
    public SecurityQuestionVo getSecurityQuestionDetailsById(@PathVariable String id) throws SecurityQuestionException {
        log.debug("Requesting all available operations by its id");
        if(StringUtils.hasText(StringUtils.trimWhitespace(id))) {
            try {
                Long actualId = Long.parseLong(id);
                log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_VALID.getValue(), id);
                SecurityQuestionVo operationDetails = service.retrieveDetailsById(actualId);
                log.debug("Responding with successful retrieval of existing operation details by id");
                return operationDetails;
            } catch (NumberFormatException e) {
                log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_INVALID.getValue(), id);
                throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
            }
        }
        log.debug(SecurityQuestionMessageTemplate.MSG_TEMPLATE_SECURITY_QUESTION_ID_EMPTY.getValue());
        throw new SecurityQuestionException(AccessErrorCode.ACCESS_ATTRIBUTE_INVALID, new Object[] { "id", id });
    }

}
