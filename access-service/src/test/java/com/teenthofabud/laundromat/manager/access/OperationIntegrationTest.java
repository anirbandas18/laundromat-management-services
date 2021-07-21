package com.teenthofabud.laundromat.manager.access;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jdk8.Jdk8Module;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.teenthofabud.core.common.data.form.PatchOperationForm;
import com.teenthofabud.core.common.data.vo.ErrorVo;
import com.teenthofabud.core.common.error.TOABErrorCode;
import com.teenthofabud.laundromat.manager.access.error.AccessErrorCode;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationEntity;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationForm;
import com.teenthofabud.laundromat.manager.access.operation.data.OperationVo;
import org.junit.Assert;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.StringUtils;

import javax.persistence.EntityManager;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

@AutoConfigureMockMvc
@ActiveProfiles("test")
@Transactional
@DirtiesContext(classMode = DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT)
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.ANY)
public class OperationIntegrationTest {

    private static final String MEDIA_TYPE_APPLICATION_JSON_PATCH = "application/json-patch+json";

    private static final Long CREATED_BY_USER_ID = 1L;

    private static final String OPERATION_URI = "/operation";
    private static final String OPERATION_URI_BY_ID = "/operation/{id}";
    private static final String OPERATION_URI_BY_NAME = "/operation/name/{name}";

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper om;

    @Autowired
    private EntityManager em;

    private OperationForm operationForm;
    private OperationVo operationVo1;
    private OperationVo operationVo2;
    private OperationVo operationVo3;
    private OperationVo operationVo4;
    private OperationEntity operationEntity1;
    private OperationEntity operationEntity2;
    private OperationEntity operationEntity3;
    private OperationEntity operationEntity4;
    private List<PatchOperationForm> patches;

    @BeforeAll
    private void setUp() {
        operationForm = new OperationForm();
        operationForm.setName("Demo Operation");
        operationForm.setDescription("This is for e2e testing of services");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample Operation"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Operation operation"));

        operationVo1 = new OperationVo();
        operationVo1.setId(1L);
        operationVo1.setActive(Boolean.TRUE);
        operationVo1.setName("Test Operation 1");
        operationVo1.setDescription("This belongs to group 1 for e2e testing");

        operationVo2 = new OperationVo();
        operationVo2.setId(2L);
        operationVo2.setActive(Boolean.TRUE);
        operationVo2.setName("Test Operation 2");
        operationVo2.setDescription("This belongs to group 1 for e2e testing");

        operationVo3 = new OperationVo();
        operationVo3.setId(3L);
        operationVo3.setActive(Boolean.TRUE);
        operationVo3.setName("Sample Operation 3");
        operationVo3.setDescription("This belongs to group 2 for e2e testing");

        operationVo4 = new OperationVo();
        operationVo4.setId(4L);
        operationVo4.setActive(Boolean.FALSE);
        operationVo4.setName("Sample Operation 4");
        operationVo4.setDescription("This belongs to group 2 for e2e testing");

        operationEntity1 = new OperationEntity();
        operationEntity1.setActive(Boolean.TRUE);
        operationEntity1.setCreatedBy(CREATED_BY_USER_ID);
        operationEntity1.setCreatedOn(LocalDateTime.now());
        operationEntity1.setModifiedBy(CREATED_BY_USER_ID);
        operationEntity1.setModifiedOn(LocalDateTime.now());
        operationEntity1.setVersion(0);
        operationEntity1.setName("Test Operation 1");
        operationEntity1.setDescription("This belongs to group 1 for e2e testing");

        operationEntity2 = new OperationEntity();
        operationEntity2.setActive(Boolean.TRUE);
        operationEntity2.setCreatedBy(CREATED_BY_USER_ID);
        operationEntity2.setCreatedOn(LocalDateTime.now());
        operationEntity2.setModifiedBy(CREATED_BY_USER_ID);
        operationEntity2.setModifiedOn(LocalDateTime.now());
        operationEntity2.setVersion(0);
        operationEntity2.setName("Test Operation 2");
        operationEntity2.setDescription("This belongs to group 1 for e2e testing");


        operationEntity3 = new OperationEntity();
        operationEntity3.setActive(Boolean.TRUE);
        operationEntity3.setCreatedBy(CREATED_BY_USER_ID);
        operationEntity3.setCreatedOn(LocalDateTime.now());
        operationEntity3.setModifiedBy(CREATED_BY_USER_ID);
        operationEntity3.setModifiedOn(LocalDateTime.now());
        operationEntity3.setVersion(0);
        operationEntity3.setName("Sample Operation 3");
        operationEntity3.setDescription("This belongs to group 2 for e2e testing");


        operationEntity4 = new OperationEntity();
        operationEntity4.setActive(Boolean.FALSE);
        operationEntity4.setCreatedBy(CREATED_BY_USER_ID);
        operationEntity4.setCreatedOn(LocalDateTime.now());
        operationEntity4.setModifiedBy(CREATED_BY_USER_ID);
        operationEntity4.setModifiedOn(LocalDateTime.now());
        operationEntity4.setVersion(0);
        operationEntity4.setName("Sample Operation 4");
        operationEntity4.setDescription("This belongs to group 2 for e2e testing");

        om.registerModule(new Jdk8Module());
        om.registerModule(new JavaTimeModule());

    }

    @BeforeEach
    private void init() {
        em.merge(operationEntity1);
        em.merge(operationEntity2);
        em.merge(operationEntity3);
        em.merge(operationEntity4);
    }

    @AfterEach
    private void destroy() {
        em.remove(operationEntity1);
        em.remove(operationEntity2);
        em.remove(operationEntity3);
        em.remove(operationEntity4);

        operationForm = new OperationForm();
        operationForm.setName("Demo Operation");
        operationForm.setDescription("This is for e2e testing of services");

        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", "Sample Operation"),
                new PatchOperationForm("replace", "/active", "true"),
                new PatchOperationForm("replace", "/description", "Patching description attribute of this Operation operation"));
    }

    @Test
    public void test_Operation_Post_ShouldReturn_201Response_And_NewOperationId_WhenPosted_WithValidOperationForm() throws Exception {
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(post(OPERATION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(operationForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CREATED.value(), mvcResult.getResponse().getStatus());
        Assert.assertTrue(StringUtils.hasText(mvcResult.getResponse().getContentAsString()));
    }

    @Test
    public void test_Operation_Post_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_WithEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        operationForm.setName("");

        mvcResult = mockMvc.perform(post(OPERATION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(operationForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Operation_Post_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_004_WhenRequested_WithDuplicateOperation() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String field1Name = "name";
        operationForm.setName("Test Operation 1");

        mvcResult = mockMvc.perform(post(OPERATION_URI)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(operationForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_Operation_Post_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenPosted_WithNoOperationForm() throws Exception {
        long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = mockMvc.perform(post(OPERATION_URI)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));

    }

    @Test
    public void test_Operation_Get_ShouldReturn_200Response_And_OperationListNaturallyOrdered_WhenRequested_ForAllOperations() throws Exception {
        MvcResult mvcResult = null;
        Set<OperationVo> studentList = new TreeSet<>(Arrays.asList(operationVo1, operationVo2, operationVo3, operationVo4));
        long expectedOperationVoCount = 4;

        mvcResult = this.mockMvc.perform(get(OPERATION_URI))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(expectedOperationVoCount, om.readValue(mvcResult.getResponse().getContentAsString(), OperationVo[].class).length);
    }

    @Test
    public void test_Operation_Get_ShouldReturn_200Response_And_OperationDetails_WhenRequested_ById() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(OPERATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(om.writeValueAsString(operationVo1), mvcResult.getResponse().getContentAsString());
        Assert.assertEquals(operationVo1.getId(), om.readValue(mvcResult.getResponse().getContentAsString(), OperationVo.class).getId());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Operation_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequestedBy_EmptyInvalidId(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(OPERATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Operation_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(get(OPERATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Operation_Get_ShouldReturn_200Response_And_MatchingOperationDetails_WhenRequested_ByName() throws Exception {
        String name = "Test";
        List<OperationVo> students = Arrays.asList(operationVo1, operationVo2);
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(get(OPERATION_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.OK.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(2, om.readValue(mvcResult.getResponse().getContentAsString(), OperationVo[].class).length);
    }

    @Test
    public void test_Operation_Get_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ByEmptyName() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(OPERATION_URI_BY_NAME, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Operation_Get_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenRequested_ByAbsentName() throws Exception {
        MvcResult mvcResult = null;
        String name = "kk";
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "name";

        mvcResult = this.mockMvc.perform(get(OPERATION_URI_BY_NAME, name))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(name));
    }

    @Test
    public void test_Operation_Delete_ShouldReturn_204Response_And_NoResponseBody_WhenDeleted_ById() throws Exception {
        Long id = 3l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(delete(OPERATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Operation_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001__WhenDeleted_ByEmptyId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(OPERATION_URI_BY_ID, " "))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Operation_Delete_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenDeleted_ByInvalidId() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(OPERATION_URI_BY_ID, "r"))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Operation_Delete_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_005_WhenDeleted_ByInactiveId() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(delete(OPERATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Operation_Delete_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenDeleted_ByAbsentId() throws Exception {
        Long id = 111l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(delete(OPERATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Operation_Put_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndOperationDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        operationForm.setName("Ferran");

        mvcResult = this.mockMvc.perform(put(OPERATION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(operationForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @ParameterizedTest
    @ValueSource(strings = { " ", "r" })
    public void test_Operation_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenUpdatedBy_EmptyInvalidId_AndOperationDetails(String id) throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(OPERATION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(operationForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Operation_Put_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ByAbsentId_AndOperationDetails() throws Exception {
        Long id = 41l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(put(OPERATION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(operationForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Operation_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_005_WhenUpdated_ByInactiveId_AndOperationDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_INACTIVE.getErrorCode();

        mvcResult = this.mockMvc.perform(put(OPERATION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(operationForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Operation_Put_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndNoOperationDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(put(OPERATION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Operation_Put_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "name";
        operationForm.setName("");

        mvcResult = mockMvc.perform(put(OPERATION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(operationForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Operation_Put_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndEmptyOperationDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "form";
        String message = "fields are expected with new values";

        mvcResult = this.mockMvc.perform(put(OPERATION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(new OperationForm())))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Operation_Put_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_004_WhenUpdated_ById_AndDuplicateOperationDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String field1Name = "name";
        operationForm.setName(operationEntity1.getName());

        mvcResult = mockMvc.perform(put(OPERATION_URI_BY_ID, id)
                .contentType(MediaType.APPLICATION_JSON)
                .content(om.writeValueAsString(operationForm)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(field1Name));
    }

    @Test
    public void test_Operation_Patch_ShouldReturn_204Response_And_NoResponseBody_WhenUpdated_ById_AndOperationDetails() throws Exception {
        Long id = 4l;
        MvcResult mvcResult = null;

        mvcResult = this.mockMvc.perform(patch(OPERATION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NO_CONTENT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals("", mvcResult.getResponse().getContentAsString());
    }

    @Test
    public void test_Operation_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenUpdated_ByEmptyId_AndOperationDetails() throws Exception {
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(OPERATION_URI_BY_ID, " ")
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
    }

    @Test
    public void test_Operation_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ByInvalidId_AndOperationDetails() throws Exception {
        String id = "r";
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(OPERATION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id));
    }

    @Test
    public void test_Operation_Patch_ShouldReturn_404Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ByAbsentId_AndOperationDetails() throws Exception {
        Long id = 411l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_NOT_FOUND.getErrorCode();
        String fieldName = "id";

        mvcResult = this.mockMvc.perform(patch(OPERATION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.NOT_FOUND.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(id.toString()));
    }

    @Test
    public void test_Operation_Patch_ShouldReturn_409Response_And_ErrorCode_LMS_ACCESS_002_WhenUpdated_ById_AndDuplicateOperationDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_EXISTS.getErrorCode();
        String fieldName = "name";
        String fieldValue = "Sample Operation 3";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", fieldValue));


        mvcResult = this.mockMvc.perform(patch(OPERATION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.CONFLICT.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldValue));
    }

    @Test
    public void test_Operation_Patch_ShouldReturn_422Response_And_ErrorCode_LMS_ACCESS_003_WhenUpdated_ById_AndNoOperationDetails() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_UNEXPECTED.getErrorCode();
        String fieldName = "patch";
        String message = "not provided";

        mvcResult = this.mockMvc.perform(patch(OPERATION_URI_BY_ID, id))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.UNPROCESSABLE_ENTITY.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(message));
    }

    @Test
    public void test_Operation_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidActive() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "active";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/active", "x"));

        mvcResult = mockMvc.perform(patch(OPERATION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Operation_Patch_ShouldReturn_400Response_And_ErrorCode_TOAB_COMMON_001_WhenRequested_ById_AndInvalidName() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = TOABErrorCode.PATCH_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "value";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/name", " "));

        mvcResult = mockMvc.perform(patch(OPERATION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

    @Test
    public void test_Operation_Patch_ShouldReturn_400Response_And_ErrorCode_LMS_ACCESS_001_WhenRequested_ById_AndInvalidDefinitionOfOperationAttribute() throws Exception {
        Long id = 1l;
        MvcResult mvcResult = null;
        String errorCode = AccessErrorCode.ACCESS_ATTRIBUTE_INVALID.getErrorCode();
        String fieldName = "path";
        patches = Arrays.asList(
                new PatchOperationForm("replace", "/x", "x"));

        mvcResult = mockMvc.perform(patch(OPERATION_URI_BY_ID, id)
                .contentType(MEDIA_TYPE_APPLICATION_JSON_PATCH)
                .content(om.writeValueAsString(patches)))
                .andDo(print())
                .andReturn();

        Assert.assertNotNull(mvcResult);
        Assert.assertEquals(HttpStatus.BAD_REQUEST.value(), mvcResult.getResponse().getStatus());
        Assert.assertEquals(errorCode, om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getCode());
        Assert.assertTrue(om.readValue(mvcResult.getResponse().getContentAsString(), ErrorVo.class).getMessage().contains(fieldName));

    }

}