;;; "Compiled" snippets and support files for `typescript-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'typescript-mode
                     '(("nextroute"
                        "import { getServerSession } from \"next-auth/next\";\nimport { authOptions } from \"@/lib/nextauth\";\nimport { PrismaClient } from \"@prisma/client\";\nimport { z } from \"zod\";\n\nconst prisma = new PrismaClient();\n\nconst querySchema = z.object({\n  ${1:id}: z.string().transform(Number).optional(),\n  ${2:status}: z.string().optional(),\n  limit: z.string().transform(Number).default('${3:50}'),\n});\n\nexport async function GET(req: Request) {\n  const session = await getServerSession(authOptions);\n  if (!session) {\n    return new Response(\"Unauthorized\", { status: 401 });\n  }\n\n  try {\n    const url = new URL(req.url);\n    const queryParams = Object.fromEntries(url.searchParams);\n    const params = querySchema.parse(queryParams);\n\n    const ${4:results} = await prisma.${5:table}.${6:findMany}({\n      where: {\n        ${7:user_id}: Number(session.user.${8:id}),\n        ${9:// Add conditions based on params}\n      },\n    });\n\n    return Response.json(${4:results}, { status: 200 });\n\n  } catch (error) {\n    if (error instanceof z.ZodError) {\n      return Response.json(\n        { error: \"Invalid query parameters\", details: error.errors },\n        { status: 400 }\n      );\n    }\n\n    console.error(\"${10:Endpoint} error:\", error);\n    return new Response(\"Internal Server Error\", { status: 500 });\n  }\n}\n\n${0:}"
                        "nextjs-api-route" nil nil nil
                        "/home/gabriel/.config/doom/snippets/typescript-mode/nextjs-api-get.yas"
                        nil nil)))


;;; Do not edit! File generated at Sat Oct 18 18:13:36 2025
