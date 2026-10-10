; CaseSensitiveRouting.cs generator — makes literal route segments case-sensitive.
; Resource types are case-sensitive in FHIR, ASP.NET Core routing matches literals
; case-insensitively. Auth-agnostic, so it lives in the shared layer like result.scm
; and is imported by every project's map.dsl; each boilerplate registers it in
; Program.cs with builder.Services.AddCaseSensitiveRouting().

(define (routing)
    (file "CaseSensitiveRouting.cs" ($<![CDATA[
"using Microsoft.AspNetCore.Routing;
using Microsoft.AspNetCore.Routing.Matching;
using Microsoft.AspNetCore.Routing.Patterns;
using Microsoft.Extensions.DependencyInjection.Extensions;

namespace "(project-name)";

/// <summary>
/// Resource types and the other literal path segments (_history, $undelete, metadata, ...) are
/// case-sensitive in FHIR, but the ASP.NET Core matcher compares route literals case-insensitively,
/// so /survey would reach the Survey handlers. This policy runs inside endpoint selection and
/// discards every candidate whose literal segments differ from the requested path in case. A
/// request that keeps no candidate is treated like any other unknown path.
/// </summary>
public sealed class CaseSensitiveRouteMatcherPolicy : MatcherPolicy, IEndpointSelectorPolicy
{
    public override int Order => 0;

    public bool AppliesToEndpoints(IReadOnlyList<Endpoint> endpoints) =>
        endpoints.Any(e => e is RouteEndpoint);

    public Task ApplyAsync(HttpContext httpContext, CandidateSet candidates)
    {
        string[]? requested = null;
        for (var i = 0; i < candidates.Count; i++)
        {
            if (!candidates.IsValidCandidate(i) || candidates[i].Endpoint is not RouteEndpoint endpoint)
                continue;
            requested ??= (httpContext.Request.Path.Value ?? string.Empty).Split('/', StringSplitOptions.RemoveEmptyEntries);
            var segments = endpoint.RoutePattern.PathSegments;
            for (var s = 0; s < segments.Count && s < requested.Length; s++)
            {
                if (segments[s].IsSimple
                    && segments[s].Parts[0] is RoutePatternLiteralPart literal
                    && !string.Equals(literal.Content, requested[s], StringComparison.Ordinal))
                {
                    candidates.SetValidity(i, false);
                    break;
                }
            }
        }
        return Task.CompletedTask;
    }
}

public static class CaseSensitiveRoutingExtensions
{
    public static IServiceCollection AddCaseSensitiveRouting(this IServiceCollection services)
    {
        services.TryAddEnumerable(ServiceDescriptor.Singleton<MatcherPolicy, CaseSensitiveRouteMatcherPolicy>());
        return services;
    }
}
"]]>)))
