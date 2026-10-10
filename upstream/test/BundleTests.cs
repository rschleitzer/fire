using System.Net;
using Fire.Client;
using Fire.Model;

namespace Fire.Test;

public class TestBatchBundle : TestBase
{
    private string ExtractId(string location, string resourceType)
    {
        var marker = $"{resourceType}/";
        var start = location.IndexOf(marker);
        if (start < 0) return "";
        start += marker.Length;
        var end = location.IndexOf('/', start);
        return end < 0 ? location[start..] : location[start..end];
    }

    [Fact]
    public async Task test_batch_create_multiple()
    {
        var bundle = new Bundle
        {
            resourceType = "Bundle",
            type = "batch",
            entry =
            [
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: "BatchTest0")
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: "BatchTest1")
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: "BatchTest2")
                }
            ]
        };

        var result = await Client.SubmitBundleAsync(bundle);

        Assert.Equal("Bundle", result.resourceType);
        Assert.Equal("batch-response", result.type);
        Assert.NotNull(result.entry);
        Assert.Equal(3, result.entry!.Count);

        foreach (var entry in result.entry)
        {
            Assert.NotNull(entry.response);
            Assert.StartsWith("201", entry.response!.status!);

            var location = entry.response.location;
            Assert.NotNull(location);
            var id = ExtractId(location!, "Patient");
            Assert.NotEmpty(id);
            Track("Patient", id);
        }
    }

    [Fact]
    public async Task test_batch_mixed_operations()
    {
        var patient = Gen.Patient(family: "BatchMixed");
        var created = await CreatePatientAsync(patient);

        var bundle = new Bundle
        {
            resourceType = "Bundle",
            type = "batch",
            entry =
            [
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: "NewBatchPatient")
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "GET", url = $"Patient/{created.id}" }
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "PUT", url = $"Patient/{created.id}" },
                    resource = new Patient
                    {
                        resourceType = "Patient",
                        id = created.id,
                        meta = created.meta,
                        identifier = created.identifier,
                        name = [new HumanName { family = "UpdatedInBatch" }],
                        gender = created.gender,
                        birthDate = created.birthDate,
                        active = created.active
                    }
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "GET", url = "Patient?family=BatchMixed" }
                }
            ]
        };

        var result = await Client.SubmitBundleAsync(bundle);

        Assert.NotNull(result.entry);
        Assert.Equal(4, result.entry!.Count);

        Assert.StartsWith("201", result.entry[0].response!.status!);
        Assert.StartsWith("200", result.entry[1].response!.status!);
        Assert.StartsWith("200", result.entry[2].response!.status!);
        Assert.StartsWith("200", result.entry[3].response!.status!);

        // Track the newly created patient from the POST entry
        var newLocation = result.entry[0].response!.location;
        if (newLocation != null)
        {
            var newId = ExtractId(newLocation, "Patient");
            if (!string.IsNullOrEmpty(newId))
                Track("Patient", newId);
        }
    }

    [Fact]
    public async Task test_batch_partial_failure()
    {
        var bundle = new Bundle
        {
            resourceType = "Bundle",
            type = "batch",
            entry =
            [
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: "BatchPartial1")
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "GET", url = "Patient/nonexistent-id-12345" }
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: "BatchPartial2")
                }
            ]
        };

        var result = await Client.SubmitBundleAsync(bundle);

        Assert.NotNull(result.entry);
        Assert.Equal(3, result.entry!.Count);

        Assert.StartsWith("201", result.entry[0].response!.status!);
        Assert.StartsWith("404", result.entry[1].response!.status!);
        Assert.StartsWith("201", result.entry[2].response!.status!);

        // Track created patients for cleanup
        foreach (var entry in new[] { result.entry[0], result.entry[2] })
        {
            var location = entry.response!.location;
            if (location != null)
            {
                var id = ExtractId(location, "Patient");
                if (!string.IsNullOrEmpty(id))
                    Track("Patient", id);
            }
        }
    }
}

public class TestTransactionBundle : TestBase
{
    private string ExtractId(string location, string resourceType)
    {
        var marker = $"{resourceType}/";
        var start = location.IndexOf(marker);
        if (start < 0) return "";
        start += marker.Length;
        var end = location.IndexOf('/', start);
        return end < 0 ? location[start..] : location[start..end];
    }

    [Fact]
    public async Task test_transaction_all_succeed()
    {
        var bundle = new Bundle
        {
            resourceType = "Bundle",
            type = "transaction",
            entry =
            [
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: "TransactionTest1")
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: "TransactionTest2")
                }
            ]
        };

        var result = await Client.SubmitBundleAsync(bundle);

        Assert.Equal("transaction-response", result.type);
        Assert.NotNull(result.entry);
        Assert.Equal(2, result.entry!.Count);

        foreach (var entry in result.entry)
        {
            Assert.StartsWith("201", entry.response!.status!);

            var location = entry.response.location;
            Assert.NotNull(location);
            var id = ExtractId(location!, "Patient");
            Assert.NotEmpty(id);
            Track("Patient", id);
        }
    }

    [Fact]
    public async Task test_transaction_with_references()
    {
        var bundle = new Bundle
        {
            resourceType = "Bundle",
            type = "transaction",
            entry =
            [
                new Bundle.Entry
                {
                    fullUrl = "urn:uuid:patient-temp-id",
                    request = new Bundle.Entry.Request { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: "RefTransaction")
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "POST", url = "Observation" },
                    resource = Gen.Observation(
                        subject: new Reference { reference = "urn:uuid:patient-temp-id" },
                        code: "8867-4",
                        codeDisplay: "Heart rate",
                        valueQuantity: Gen.Qty(75, "bpm", "http://unitsofmeasure.org", "/min"))
                }
            ]
        };

        var result = await Client.SubmitBundleAsync(bundle);

        Assert.NotNull(result.entry);
        Assert.Equal(2, result.entry!.Count);

        foreach (var entry in result.entry)
        {
            Assert.StartsWith("201", entry.response!.status!);
        }

        // Track created resources for cleanup
        foreach (var entry in result.entry)
        {
            var location = entry.response!.location;
            if (location == null) continue;
            if (location.Contains("Observation/"))
            {
                var id = ExtractId(location, "Observation");
                if (!string.IsNullOrEmpty(id))
                    Track("Observation", id);
            }
            else if (location.Contains("Patient/"))
            {
                var id = ExtractId(location, "Patient");
                if (!string.IsNullOrEmpty(id))
                    Track("Patient", id);
            }
        }
    }

    [Fact]
    public async Task test_transaction_rollback_on_error()
    {
        var uniqueFamily = $"RollbackTest-{Guid.NewGuid():N}";

        // Count patients before transaction
        var beforeBundle = await Client.Patient()
            .Where(p => p.Family == uniqueFamily).ExecuteAsync();
        var beforeCount = beforeBundle.total ?? 0;

        var bundle = new
        {
            resourceType = "Bundle",
            type = "transaction",
            entry = new object[]
            {
                new
                {
                    request = new { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: uniqueFamily)
                },
                new
                {
                    request = new { method = "POST", url = "Patient" },
                    resource = new
                    {
                        resourceType = "Patient",
                        active = "invalid-boolean",
                        name = new[] { new { family = "ShouldFail" } }
                    }
                }
            }
        };

        var response = await PostBundleAsync(bundle);

        Assert.True(
            response.StatusCode is HttpStatusCode.BadRequest or HttpStatusCode.UnprocessableEntity,
            $"Transaction with validation error should fail, got {(int)response.StatusCode}");

        // Verify rollback: no patients should have been created
        var afterBundle = await Client.Patient()
            .Where(p => p.Family == uniqueFamily).ExecuteAsync();
        var afterCount = afterBundle.total ?? 0;

        Assert.Equal(beforeCount, afterCount);
    }

    [Fact]
    public async Task test_transaction_conditional_create()
    {
        var identifierValue = $"TXN-COND-{Guid.NewGuid()}";

        var bundle = new Bundle
        {
            resourceType = "Bundle",
            type = "transaction",
            entry =
            [
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request
                    {
                        method = "POST",
                        url = "Patient",
                        ifNoneExist = $"identifier={identifierValue}"
                    },
                    resource = Gen.Patient(
                        family: "ConditionalTxn",
                        identifier: [Gen.Id(system: "http://hospital.org/transaction", value: identifierValue)])
                }
            ]
        };

        // First transaction - should create
        var result1 = await Client.SubmitBundleAsync(bundle);
        Assert.NotNull(result1.entry);
        var location1 = result1.entry![0].response!.location;
        Assert.NotNull(location1);

        var createdId = ExtractId(location1!, "Patient");
        Assert.NotEmpty(createdId);
        Track("Patient", createdId);

        // Second transaction with same identifier - should not create duplicate
        var result2 = await Client.SubmitBundleAsync(bundle);
        Assert.NotNull(result2.entry);
        var status2 = result2.entry![0].response!.status!;
        Assert.StartsWith("200", status2);
    }
}

public class TestBundleEdgeCases : TestBase
{
    private string ExtractId(string location, string resourceType)
    {
        var marker = $"{resourceType}/";
        var start = location.IndexOf(marker);
        if (start < 0) return "";
        start += marker.Length;
        var end = location.IndexOf('/', start);
        return end < 0 ? location[start..] : location[start..end];
    }

    [Fact]
    public async Task test_empty_batch()
    {
        var bundle = new Bundle
        {
            resourceType = "Bundle",
            type = "batch",
            entry = []
        };

        var result = await Client.SubmitBundleAsync(bundle);

        Assert.Equal("batch-response", result.type);
        var entryCount = result.entry?.Count ?? 0;
        Assert.Equal(0, entryCount);
    }

    [Fact]
    public async Task test_large_batch()
    {
        var entries = Enumerable.Range(0, 20).Select(i => new Bundle.Entry
        {
            request = new Bundle.Entry.Request { method = "POST", url = "Patient" },
            resource = Gen.Patient(family: $"LargeBatch{i}")
        }).ToList();

        var bundle = new Bundle
        {
            resourceType = "Bundle",
            type = "batch",
            entry = entries
        };

        var result = await Client.SubmitBundleAsync(bundle);

        Assert.NotNull(result.entry);
        Assert.Equal(20, result.entry!.Count);

        foreach (var entry in result.entry)
        {
            Assert.StartsWith("201", entry.response!.status!);

            var location = entry.response.location;
            if (location != null)
            {
                var id = ExtractId(location, "Patient");
                if (!string.IsNullOrEmpty(id))
                    Track("Patient", id);
            }
        }
    }

    [Fact]
    public async Task test_bundle_invalid_entry()
    {
        var bundle = new
        {
            resourceType = "Bundle",
            type = "batch",
            entry = new object[]
            {
                new
                {
                    request = new { method = "POST", url = "Patient" },
                    resource = Gen.Patient(family: "ValidEntry")
                },
                new
                {
                    request = new { method = "POST", url = "Patient" },
                    resource = new
                    {
                        name = new[] { new { family = "Invalid" } }
                    }
                }
            }
        };

        var response = await PostBundleAsync(bundle);

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        var result = await DeserializeAsync<Bundle>(response);

        Assert.NotNull(result.entry);
        Assert.Equal(2, result.entry!.Count);

        Assert.StartsWith("201", result.entry[0].response!.status!);
        Assert.StartsWith("400", result.entry[1].response!.status!);

        // Track the successfully created patient
        var location = result.entry[0].response!.location;
        if (location != null)
        {
            var id = ExtractId(location, "Patient");
            if (!string.IsNullOrEmpty(id))
                Track("Patient", id);
        }
    }

    [Fact]
    public async Task test_transaction_ordering()
    {
        var patientId = Guid.NewGuid().ToString();

        var bundle = new Bundle
        {
            resourceType = "Bundle",
            type = "transaction",
            entry =
            [
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "PUT", url = $"Patient/{patientId}" },
                    resource = Gen.Patient(
                        id: patientId,
                        family: "OrderTest",
                        given: "V1")
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "PUT", url = $"Patient/{patientId}" },
                    resource = Gen.Patient(
                        id: patientId,
                        family: "OrderTest",
                        given: "V2")
                },
                new Bundle.Entry
                {
                    request = new Bundle.Entry.Request { method = "GET", url = $"Patient/{patientId}" }
                }
            ]
        };

        var result = await Client.SubmitBundleAsync(bundle);

        Assert.NotNull(result.entry);
        Assert.Equal(3, result.entry!.Count);

        Track("Patient", patientId);

        // The GET response (entry[2]) should have the V2 data from the second PUT
        var readResource = result.entry[2].resource;
        Assert.NotNull(readResource);
        var readPatient = readResource as Patient;
        Assert.NotNull(readPatient);
        Assert.NotNull(readPatient!.name);
        Assert.NotEmpty(readPatient.name!);
        Assert.Equal("V2", readPatient.name![0].given?[0]);
    }
}
